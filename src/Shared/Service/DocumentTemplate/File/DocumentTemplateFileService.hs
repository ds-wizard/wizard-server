module Shared.Service.DocumentTemplate.File.DocumentTemplateFileService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.WizardDocumentTemplateDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Service.DocumentTemplate.DocumentTemplateValidation
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileMapper
import Shared.Util.Uuid

getFiles :: WizardRequestContextC s m => U.UUID -> m [DocumentTemplateFileList]
getFiles dtUuid = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  findFileListsByDocumentTemplateUuid dtUuid

getFile :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateFile
getFile fileUuid = do
  checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
  findFileByUuid fileUuid

createFile :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFileChangeDTO -> m DocumentTemplateFile
createFile dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    validateFileAndAssetUniqueness Nothing dtUuid reqDto.fileName
    fUuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let newFile = fromChangeDTO reqDto dtUuid fUuid tenantUuid now now
    insertFile newFile
    touchDocumentTemplateByUuid newFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fUuid
    return newFile

modifyFile :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFileChangeDTO -> m DocumentTemplateFile
modifyFile fileUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    file <- findFileByUuid fileUuid
    validateFileAndAssetUniqueness (Just file.uuid) file.documentTemplateUuid reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedFile = fromChangeDTO reqDto file.documentTemplateUuid file.uuid file.tenantUuid file.createdAt now
    updateFileByUuid updatedFile
    touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

modifyFileContent :: WizardRequestContextC s m => U.UUID -> String -> m DocumentTemplateFile
modifyFileContent fileUuid content =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    file <- findFileByUuid fileUuid
    now <- liftIO getCurrentTime
    let updatedFile = fromContentChangeDTO file content now
    updateFileByUuid updatedFile
    touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

duplicateFile :: WizardRequestContextC s m => U.UUID -> DocumentTemplateFile -> m DocumentTemplateFile
duplicateFile newDtUuid file = do
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedFile = fromDuplicateDTO file newDtUuid aUuid now
  insertFile updatedFile
  touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
  return updatedFile

deleteFile :: WizardRequestContextC s m => U.UUID -> m ()
deleteFile fileUuid =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    file <- findFileByUuid fileUuid
    deleteFileById file.uuid
    touchDocumentTemplateByUuid file.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return ()
