module Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetCreateDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Util.Uuid

getAssets :: WizardRequestContextC s m => U.UUID -> m [DocumentTemplateAssetDTO]
getAssets dtUuid = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  assets <- findAssetsByDocumentTemplateUuid dtUuid
  now <- liftIO getCurrentTime
  traverse
    ( \asset -> do
        let expirationInSeconds = 60
        let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
        url <- presignGetAssetUrl asset.documentTemplateUuid asset.uuid expirationInSeconds
        return $ toDTO asset url urlExpiration
    )
    assets

getAsset :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateAssetDTO
getAsset assetUuid = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  asset <- findAssetById assetUuid
  let expirationInSeconds = 60
  now <- liftIO getCurrentTime
  let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
  url <- presignGetAssetUrl asset.documentTemplateUuid asset.uuid expirationInSeconds
  return $ toDTO asset url urlExpiration

getAssetContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (DocumentTemplateAsset, BS.ByteString)
getAssetContent dtUuid assetUuid = do
  asset <- findAssetById assetUuid
  content <- retrieveAsset dtUuid asset.uuid
  return (asset, content)

createAsset :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAssetCreateDTO -> m DocumentTemplateAssetDTO
createAsset dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    validateFileAndAssetUniqueness Nothing dtUuid reqDto.fileName
    aUuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let newAsset = fromCreateDTO dtUuid aUuid reqDto.fileName reqDto.contentType fileSize tenantUuid now now
    insertAsset newAsset
    touchDocumentTemplateByUuid newAsset.documentTemplateUuid
    putAsset dtUuid aUuid reqDto.contentType reqDto.content
    deleteTemporalDocumentsByAssetUuid aUuid
    let expirationInSeconds = 60
    now <- liftIO getCurrentTime
    let urlExpiration = addUTCTime (realToFrac expirationInSeconds) now
    url <- presignGetAssetUrl newAsset.documentTemplateUuid aUuid expirationInSeconds
    return $ toDTO newAsset url urlExpiration

modifyAsset :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAssetChangeDTO -> m DocumentTemplateAsset
modifyAsset assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    asset <- findAssetById assetUuid
    validateFileAndAssetUniqueness (Just asset.uuid) asset.documentTemplateUuid reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedAsset = fromChangeDTO asset reqDto now
    updateAssetById updatedAsset
    touchDocumentTemplateByUuid asset.documentTemplateUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    return updatedAsset

modifyAssetContent :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAssetCreateDTO -> m DocumentTemplateAsset
modifyAssetContent assetUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    checkStorageSize (fromIntegral . BS.length $ reqDto.content)
    now <- liftIO getCurrentTime
    asset <- findAssetById assetUuid
    let fileSize = fromIntegral . BS.length $ reqDto.content
    let updatedAsset = fromChangeContentDTO asset reqDto.fileName reqDto.contentType fileSize now
    updateAssetById updatedAsset
    touchDocumentTemplateByUuid updatedAsset.documentTemplateUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    putAsset asset.documentTemplateUuid assetUuid reqDto.contentType reqDto.content
    return updatedAsset

duplicateAsset :: WizardRequestContextC s m => U.UUID -> DocumentTemplateAsset -> m DocumentTemplateAsset
duplicateAsset newDtUuid asset = do
  content <- retrieveAsset asset.documentTemplateUuid asset.uuid
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedAsset = fromDuplicateDTO asset newDtUuid aUuid now
  insertAsset updatedAsset
  touchDocumentTemplateByUuid asset.documentTemplateUuid
  putAsset newDtUuid aUuid updatedAsset.contentType content
  return updatedAsset

deleteAsset :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteAsset dtUuid assetUuid =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    asset <- findAssetById assetUuid
    deleteAssetById asset.uuid
    removeAsset dtUuid assetUuid
    touchDocumentTemplateByUuid dtUuid
    deleteTemporalDocumentsByAssetUuid assetUuid
    return ()
