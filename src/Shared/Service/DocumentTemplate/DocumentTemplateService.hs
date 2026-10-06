module Shared.Service.DocumentTemplate.DocumentTemplateService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Constant.DocumentTemplate
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO
import Shared.Database.DAO.Library.LibraryDependentDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO (unsetDocumentTemplateInProjects)
import Shared.Database.DAO.Registry.RegistryTemplateDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateSuggestion
import Shared.Model.Error.Error
import Shared.Model.Library.LibraryDependents
import Shared.Model.Settings.Settings
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Service.Acl.LibraryAcl
import Shared.Service.Document.DocumentCleanService
import Shared.Service.DocumentTemplate.DocumentTemplateAcl
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import qualified Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleMapper as DocumentTemplateLocaleMapper
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import Shared.Service.DocumentTemplate.WizardDocumentTemplateUtil
import Shared.Service.Library.LibraryDependentMapper
import Shared.Service.Settings.OrganizationSettingsService

getDocumentTemplatesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page DocumentTemplateSimpleDTO)
getDocumentTemplatesPage mId mQuery mOutdated pageable sort = do
  tcRegistry <- getCurrentSettingsRegistry
  if mOutdated == Just True && not tcRegistry.enabled
    then return $ Page "documentTemplates" (PageMetadata 0 0 0 0) []
    else do
      templates <- findDocumentTemplatesPage mId mQuery mOutdated Nothing pageable sort
      return . fmap (toSimpleDTO' tcRegistry.enabled) $ templates

getDocumentTemplateSuggestions :: WizardRequestContextC s m => Maybe U.UUID -> Bool -> Maybe DocumentTemplatePhase -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page DocumentTemplateSuggestionDTO)
getDocumentTemplateSuggestions mPkgUuid includeUnsupportedMetamodelVersion mPhase mQuery mNonEditable pageable sort = do
  mPkgId <-
    case mPkgUuid of
      Just pkgUuid -> do
        pkg <- findPackageByUuid pkgUuid
        return $ Just $ createCoordinate pkg
      Nothing -> return Nothing
  tmls <- findDocumentTemplatesSuggestions mQuery mNonEditable
  let entities = filterDocumentTemplatesInGroup mPkgId tmls
  return $ toSuggestionDTOPage entities pageable
  where
    filterDocumentTemplatesInGroup :: Maybe Coordinate -> [DocumentTemplateSuggestion] -> [DocumentTemplateSuggestion]
    filterDocumentTemplatesInGroup mPkgId =
      filter (\dt -> includeUnsupportedMetamodelVersion || isDocumentTemplateSupported dt.metamodelVersion)
        . filter (isDocumentTemplateInPhase mPhase)
        . filterDocumentTemplates mPkgId

getDocumentTemplatesDto :: WizardRequestContextC s m => [(String, String)] -> m [DocumentTemplateSuggestionDTO]
getDocumentTemplatesDto queryParams = do
  dts <- findDocumentTemplatesFiltered queryParams
  traverse
    ( \dt -> do
        formats <- findDocumentTemplateFormats dt.uuid
        locales <- findDocumentTemplateLocalesByDocumentTemplateUuid dt.uuid
        return $ toSuggestionDTO' dt formats (fmap DocumentTemplateLocaleMapper.toList locales)
    )
    dts

getDocumentTemplateByUuidAndPackageId :: WizardRequestContextC s m => U.UUID -> U.UUID -> U.UUID -> m DocumentTemplate
getDocumentTemplateByUuidAndPackageId documentTemplateUuid pkgUuid workspaceUuid = do
  templates <- findDocumentTemplatesInWorkspace (Just workspaceUuid)
  pkg <- findPackageByUuid pkgUuid
  let dts = filterDocumentTemplates (Just . createCoordinate $ pkg) templates
  case L.find (\dt -> dt.uuid == documentTemplateUuid) dts of
    Just dt -> return dt
    Nothing -> throwError . NotExistsError $ _ERROR_VALIDATION__TEMPLATE_ABSENCE

getDocumentTemplateByUuidDto :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateDetailDTO
getDocumentTemplateByUuidDto uuid = do
  tml <- findDocumentTemplateByUuid uuid
  formats <- findDocumentTemplateFormats uuid
  versions <- getDocumentTemplateVersions tml
  tmlRs <- findRegistryTemplates
  serverConfig <- asks (.serverConfig')
  let registryLink = buildRegistryTemplateUrl serverConfig.registry.clientUrl tml tmlRs
  usableKnowledgeModels <- findUsablePackagesForDocumentTemplate tml.uuid
  tcRegistry <- getCurrentSettingsRegistry
  locales <- findDocumentTemplateLocalesByDocumentTemplateUuid uuid
  return $ toDetailDTO tml formats tcRegistry.enabled tmlRs versions registryLink usableKnowledgeModels (fmap DocumentTemplateLocaleMapper.toList locales)

modifyDocumentTemplate :: WizardRequestContextC s m => U.UUID -> DocumentTemplateChangeDTO -> m DocumentTemplateDetailDTO
modifyDocumentTemplate uuid reqDto =
  runInTransaction $ do
    checkManagePermissionToDocumentTemplate uuid
    validateChangeDto uuid reqDto
    tml <- findDocumentTemplateByUuid uuid
    let templateUpdated = fromChangeDTO reqDto tml
    updateDocumentTemplateById templateUpdated
    deleteTemporalDocumentsByDocumentTemplateUuid uuid
    getDocumentTemplateByUuidDto uuid

getDocumentTemplateDependents :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m LibraryDependents
getDocumentTemplateDependents uuid mAllVersions = do
  checkManagePermissionToDocumentTemplate uuid
  dtUuids <- getDocumentTemplateUuidsToDelete uuid mAllVersions
  toLibraryDependents <$> findDocumentTemplateDependents dtUuids

deleteDocumentTemplate :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m ()
deleteDocumentTemplate uuid mAllVersions =
  runInTransaction $ do
    checkManagePermissionToDocumentTemplate uuid
    dtUuids <- getDocumentTemplateUuidsToDelete uuid mAllVersions
    dependents <- toLibraryDependents <$> findDocumentTemplateDependents dtUuids
    checkDeleteAllowed dependents
    traverse_ deleteDocumentTemplateWithAssets dtUuids

-- --------------------------------
-- PRIVATE
-- --------------------------------
getDocumentTemplateVersions :: WizardRequestContextC s m => DocumentTemplate -> m [(U.UUID, String)]
getDocumentTemplateVersions tml = do
  allTmls <- findDocumentTemplatesByIdInWorkspace tml.id tml.workspaceUuid
  return . fmap (\t -> (t.uuid, t.version)) . filter (\t -> t.phase == ReleasedDocumentTemplatePhase || t.phase == DeprecatedDocumentTemplatePhase) $ allTmls

getDocumentTemplateUuidsToDelete :: WizardRequestContextC s m => U.UUID -> Maybe Bool -> m [U.UUID]
getDocumentTemplateUuidsToDelete uuid mAllVersions = do
  tml <- findDocumentTemplateByUuid uuid
  case mAllVersions of
    Just True -> fmap (.uuid) . filter (\t -> t.phase /= DraftDocumentTemplatePhase) <$> findDocumentTemplatesByIdInWorkspace tml.id tml.workspaceUuid
    _ -> return [tml.uuid]

deleteDocumentTemplateWithAssets :: WizardRequestContextC s m => U.UUID -> m ()
deleteDocumentTemplateWithAssets uuid = do
  assets <- findAssetsByDocumentTemplateUuid uuid
  now <- liftIO getCurrentTime
  cleanTemporallyDocumentsForTemplate uuid
  void $ unsetDocumentTemplateInProjects uuid now
  void $ deleteDocumentTemplateByUuid uuid
  traverse_ (removeAsset uuid . (.uuid)) assets
