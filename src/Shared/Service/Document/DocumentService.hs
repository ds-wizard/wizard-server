module Shared.Service.Document.DocumentService where

import Control.Monad (join, void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Constant.Component
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public (_ERROR_VALIDATION__FORBIDDEN)
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.PreviewSource
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.S3.Document.DocumentS3
import Shared.Service.Document.Context.DocumentContextService
import Shared.Service.Document.DocumentAcl
import Shared.Service.Document.DocumentAuditLog
import Shared.Service.Document.DocumentMapper
import Shared.Service.Document.DocumentUtil
import Shared.Service.DocumentTemplate.DocumentTemplateService
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation
import Shared.Service.KnowledgeModel.Editor.EditorAcl
import Shared.Service.Project.ProjectAcl
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Logger
import Shared.Util.Uuid

getDocumentsPageDto :: WizardRequestContextC s m => Maybe U.UUID -> Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page DocumentDTO)
getDocumentsPageDto mProjectUuid mDocumentTemplateUuid mQuery pageable sort = do
  checkPermission _PROJECTS_EDIT_ROLE_PERMISSION
  docPage <- findDocumentsPage True mProjectUuid Nothing mDocumentTemplateUuid mQuery pageable sort
  enhanceDocuments docPage

getDocumentsForProject :: WizardRequestContextC s m => U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page DocumentDTO)
getDocumentsForProject projectUuid mQuery pageable sort = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToDoc' project
  docPage <- findDocumentsPage False (Just projectUuid) (Just project.name) Nothing mQuery pageable sort
  enhanceDocuments docPage

getDocumentByUuid :: WizardRequestContextC s m => U.UUID -> m DocumentDTO
getDocumentByUuid docUuid = do
  doc <- findDocumentByUuid docUuid
  project <- maybe (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Read Document") findProjectByUuid doc.projectUuid
  checkViewPermissionToDoc' project
  tml <- findDocumentTemplateByUuid doc.documentTemplateUuid
  format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid doc.documentTemplateUuid doc.formatUuid
  mVersion <- traverse (findProjectVersionByEventUuid' project.uuid) doc.projectEventUuid
  submissions <- findDocumentSubmissions doc
  return $ toDTOWithDocTemplate doc project (fmap (.name) (join mVersion)) submissions tml format

createDocument :: WizardRequestContextC s m => DocumentCreateDTO -> m DocumentDTO
createDocument reqDto =
  runInTransaction $ do
    checkEditPermissionToDoc (Just reqDto.projectUuid)
    checkDocumentLimit
    checkStorageSize 0
    project <- findProjectByUuid reqDto.projectUuid
    tml <- getDocumentTemplateByUuidAndPackageId reqDto.documentTemplateUuid project.knowledgeModelPackageUuid project.workspaceUuid
    format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid reqDto.documentTemplateUuid reqDto.formatUuid
    validateMetamodelVersion tml
    validateLanguageAvailability tml reqDto.language
    (ctx, mCutOffEventUuid) <- createProjectDocumentContext project reqDto.projectEventUuid
    uuid <- liftIO generateUuid
    mCurrentUser <- asks (.currentUser')
    now <- liftIO getCurrentTimeInMicroseconds
    let doc = fromCreateDTO reqDto uuid (Just ctx.hash) mCutOffEventUuid mCurrentUser project.tenantUuid project.workspaceUuid now
    insertDocument doc
    al_createDocument doc project tml format
    publishToPersistentCommandQueue doc tml format ctx
    return $ toDTOWithDocTemplate doc project Nothing [] tml format

deleteDocument :: WizardRequestContextC s m => U.UUID -> m ()
deleteDocument docUuid =
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToDoc doc.projectUuid
    al_deleteDocument doc
    void $ deleteDocumentByUuid docUuid

downloadDocument :: WizardRequestContextC s m => Int -> U.UUID -> m TemporaryFileDTO
downloadDocument expiration docUuid = fst <$> downloadDocumentWithContent expiration docUuid

downloadDocumentWithContent :: WizardRequestContextC s m => Int -> U.UUID -> m (TemporaryFileDTO, BS.ByteString)
downloadDocumentWithContent expiration docUuid = do
  runInTransaction $ do
    doc <- findDocumentByUuid docUuid
    checkViewPermissionToDoc doc.projectUuid
    content <- retrieveDocumentContent docUuid
    let fileName = fromMaybe "export" doc.fileName
    let contentType = fromMaybe "text/plain" doc.contentType
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile expiration fileName "application/octet-stream" mCurrentUserUuid (BSL.fromStrict content)
    return (TemporaryFileMapper.toDTO url contentType, content)

createDocumentPreviewForProject :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Document, TemporaryFileDTO)
createDocumentPreviewForProject projectUuid mDocumentUuid =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkViewPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
    case (project.documentTemplateUuid, project.formatUuid) of
      (Just dtUuid, Just formatUuid) -> do
        tml <- getDocumentTemplateByUuidAndPackageId dtUuid project.knowledgeModelPackageUuid project.workspaceUuid
        format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtUuid formatUuid
        resumeOrCreatePreview mDocumentUuid (isPreviewOf (ProjectPreviewSource project) tml format) $ do
          (ctx, mCutOffEventUuid) <- createProjectDocumentContext project Nothing
          createDocumentPreview (ProjectPreviewSource project) tml format project.documentTemplateLanguage mCutOffEventUuid ctx
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP

createDocumentPreviewForDocTmlDraft :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Document, TemporaryFileDTO)
createDocumentPreviewForDocTmlDraft dtUuid mDocumentUuid =
  runInTransaction $ do
    draftData <- findDraftDataByUuid dtUuid
    case (draftData.projectUuid, draftData.knowledgeModelEditorUuid, draftData.formatUuid) of
      (Just projectUuid, _, Just formatUuid) -> do
        draft <- findDraftByUuid dtUuid
        format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtUuid formatUuid
        project <- findProjectByUuid projectUuid
        checkViewPermissionToProject project.workspaceUuid project.visibility project.sharing project.permissions
        resumeOrCreatePreview mDocumentUuid (isPreviewOf (ProjectPreviewSource project) draft format) $ do
          (ctx, mCutOffEventUuid) <- createProjectDocumentContext project Nothing
          createDocumentPreview (ProjectPreviewSource project) draft format Nothing mCutOffEventUuid ctx
      (_, Just kmEditorUuid, Just formatUuid) -> do
        draft <- findDraftByUuid dtUuid
        format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtUuid formatUuid
        checkPermissionToEditor kmEditorUuid
        editor <- findKnowledgeModelEditorByUuid kmEditorUuid
        resumeOrCreatePreview mDocumentUuid (isPreviewOf (KnowledgeModelEditorPreviewSource editor) draft format) $ do
          ctx <- createKnowledgeModelEditorDocumentContext editor
          createDocumentPreview (KnowledgeModelEditorPreviewSource editor) draft format Nothing Nothing ctx
      _ -> throwError $ UserError _ERROR_SERVICE_DOCUMENT__PROJECT_OR_FORMAT_NOT_SET_UP

createDocumentPreview :: WizardRequestContextC s m => PreviewSource -> DocumentTemplate -> DocumentTemplateFormatSimple -> Maybe String -> Maybe U.UUID -> DocumentContext -> m (Document, TemporaryFileDTO)
createDocumentPreview source dt format mLanguage mEventUuid ctx = do
  logDebugI _CMP_SERVICE ("Context hash: " ++ ctx.hash)
  allDocs <- findDocumentsForCurrentTenantFiltered (previewSourceFilter source : [("context_hash", ctx.hash), ("durability", "TemporallyDocumentDurability")])
  let docs = filter (matchesDocumentKey (previewSourceName source) dt.uuid format.uuid mLanguage) allDocs
  case filter isFinishedDocument docs of
    (doc : _) -> do
      logInfoI _CMP_SERVICE "Retrieving from cache"
      toPreviewResult doc
    [] ->
      case filter isPendingDocument docs of
        (doc : _) -> do
          logInfoI _CMP_SERVICE "Waiting to generation"
          return (doc, TemporaryFileMapper.emptyFileDTO)
        _ -> do
          logInfoI _CMP_SERVICE "Generating new preview"
          validateMetamodelVersion dt
          dUuid <- liftIO generateUuid
          mCurrentUser <- asks (.currentUser')
          now <- liftIO getCurrentTimeInMicroseconds
          let doc = fromTemporallyCreateDTO dUuid source mEventUuid dt.uuid format.uuid mLanguage (Just ctx.hash) mCurrentUser now
          insertDocument doc
          publishToPersistentCommandQueue doc dt format ctx
          return (doc, TemporaryFileMapper.emptyFileDTO)

publishToPersistentCommandQueue :: WizardRequestContextC s m => Document -> DocumentTemplate -> DocumentTemplateFormatSimple -> DocumentContext -> m ()
publishToPersistentCommandQueue doc dt format ctx = do
  mCreatedBy <- traverse findUserByUuid doc.createdBy
  mLocale <- maybe (return Nothing) (findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' doc.documentTemplateUuid) doc.language
  limits <- findTenantLimits doc.tenantUuid
  pUuid <- liftIO generateUuid
  let docContext = withDocument doc dt format mCreatedBy mLocale ctx
  insertPersistentCommand (toDocPersistentCommand pUuid docContext limits doc)
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
resumeOrCreatePreview :: WizardRequestContextC s m => Maybe U.UUID -> (Document -> Bool) -> m (Document, TemporaryFileDTO) -> m (Document, TemporaryFileDTO)
resumeOrCreatePreview Nothing _ create = create
resumeOrCreatePreview (Just documentUuid) belongsToSource create = do
  mDoc <- findDocumentByUuid' documentUuid
  case mDoc of
    Just doc | doc.durability == TemporallyDocumentDurability && belongsToSource doc -> toPreviewResult doc
    _ -> create

toPreviewResult :: WizardRequestContextC s m => Document -> m (Document, TemporaryFileDTO)
toPreviewResult doc
  | doc.state == DoneDocumentState = do
      let expirationInSeconds = 60
      link <- presignGetDocumentUrl doc.uuid expirationInSeconds
      return (doc, TemporaryFileDTO link (fromMaybe "text/plain" doc.contentType))
  | otherwise = return (doc, TemporaryFileMapper.emptyFileDTO)

previewSourceFilter :: PreviewSource -> (String, String)
previewSourceFilter (ProjectPreviewSource project) = ("project_uuid", U.toString project.uuid)
previewSourceFilter (KnowledgeModelEditorPreviewSource editor) = ("knowledge_model_editor_uuid", U.toString editor.uuid)

isPreviewOf :: PreviewSource -> DocumentTemplate -> DocumentTemplateFormatSimple -> Document -> Bool
isPreviewOf source dt format doc =
  doc.documentTemplateUuid == dt.uuid
    && doc.formatUuid == format.uuid
    && case source of
      ProjectPreviewSource project -> doc.projectUuid == Just project.uuid
      KnowledgeModelEditorPreviewSource editor -> doc.knowledgeModelEditorUuid == Just editor.uuid
