module Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.WizardCommon
import Shared.Integration.Http.Registry.Runner
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Model.Error.Error
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import Shared.Service.DocumentTemplate.Locale.Pot.PotFileService
import qualified Shared.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Shared.Service.TemporaryFile.TemporaryFileService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.String
import Shared.Util.Uuid

getTemporaryFileWithBundle :: WizardRequestContextC s m => U.UUID -> m TemporaryFileDTO
getTemporaryFileWithBundle dtUuid =
  runInTransaction $ do
    (coordinate, bundle) <- exportBundle dtUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.zip" [show coordinate]) "application/octet-stream" mCurrentUserUuid bundle
    return $ TemporaryFileMapper.toDTO url "application/zip"

exportBundle :: WizardRequestContextC s m => U.UUID -> m (Coordinate, BSL.ByteString)
exportBundle dtUuid =
  runInTransaction $ do
    dt <- findDocumentTemplateByUuid dtUuid
    when
      dt.nonEditable
      (throwError . UserError $ _ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML)
    formats <- findDocumentTemplateFormats dtUuid
    files <- findFilesByDocumentTemplateUuid dtUuid
    assets <- findAssetsByDocumentTemplateUuid dtUuid
    assetContents <- traverse (findAsset dt.uuid) assets
    auditBundleExport (createCoordinate dt)
    return (createCoordinate dt, toDocumentTemplateArchive (toBundle dt formats files assets) assetContents)

pullBundleFromRegistry :: WizardRequestContextC s m => Coordinate -> m DocumentTemplateSimple
pullBundleFromRegistry coordinate =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
    checkDocumentTemplateLimit coordinate.organizationId coordinate.entityId
    tb <- catchError (retrieveDocumentTemplateBundleByCoordinate coordinate) handleError
    importAndConvertBundle tb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_TB__PULL_NON_EXISTING_TML (show coordinate)
        else throwError error

importAndConvertBundle :: WizardRequestContextC s m => BSL.ByteString -> Bool -> m DocumentTemplateSimple
importAndConvertBundle contentS fromRegistry =
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      checkPermission _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION
      checkDocumentTemplateLimit bundle.organizationId bundle.templateId
      let assetSize = foldl (\acc (_, content) -> acc + (fromIntegral . BS.length $ content)) 0 assetContents
      checkStorageSize assetSize
      uuid <- liftIO generateUuid
      tenantUuid <- asks (.tenantUuid')
      let dt = fromBundle bundle uuid tenantUuid
      validateNewDocumentTemplate dt True
      deleteOldDocumentTemplateIfPresent bundle
      traverse_ (\(a, content) -> putAsset dt.uuid a.uuid a.contentType content) assetContents
      insertDocumentTemplate dt
      traverse_ (insertDocumentTemplateFormat . fromFormatDTO dt.uuid tenantUuid dt.createdAt dt.updatedAt) bundle.formats
      traverse_ (insertFile . fromFileDTO dt.uuid tenantUuid dt.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO dt.uuid (fromIntegral . BS.length $ content) tenantUuid dt.createdAt assetDto
        )
        assetContents
      publishGeneratePotFileCommand dt
      if fromRegistry
        then auditBundlePullFromRegistry (createCoordinate dt)
        else auditBundleImportFromFile (createCoordinate dt)
      return . toSimple $ dt
    Left error -> throwError error

deleteOldDocumentTemplateIfPresent :: WizardRequestContextC s m => DocumentTemplateBundleDTO -> m ()
deleteOldDocumentTemplateIfPresent bundle =
  runInTransaction $ do
    let coordinate = createCoordinate bundle
    mOldDt <- findDocumentTemplateByCoordinate' coordinate
    case mOldDt of
      Just oldDt -> do
        oldAssets <- findAssetsByDocumentTemplateUuid oldDt.uuid
        traverse_ (\a -> removeAsset oldDt.uuid a.uuid) oldAssets
        void $ deleteDocumentTemplateByUuid oldDt.uuid
      Nothing -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAsset -> m (DocumentTemplateAsset, BS.ByteString)
findAsset dtUuid asset = do
  content <- retrieveAsset dtUuid asset.uuid
  return (asset, content)
