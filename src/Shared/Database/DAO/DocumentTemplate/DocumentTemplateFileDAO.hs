module Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFile ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFileList ()
import Shared.Model.Context.RequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Util.String

entityName = "document_template_file"

findFilesByDocumentTemplateUuid :: RequestContextC s sc m => U.UUID -> m [DocumentTemplateFile]
findFilesByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findFileListsByDocumentTemplateUuid :: RequestContextC s sc m => U.UUID -> m [DocumentTemplateFileList]
findFileListsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsByFn "uuid, file_name, created_at, updated_at" entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findFilesByDocumentTemplateUuidAndFileName :: RequestContextC s sc m => U.UUID -> String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateUuidAndFileName documentTemplateUuid fileName = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("file_name", fileName)]

findFileByUuid :: RequestContextC s sc m => U.UUID -> m DocumentTemplateFile
findFileByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertFile :: RequestContextC s sc m => DocumentTemplateFile -> m Int64
insertFile file = do
  createInsertFn entityName file

updateFileByUuid :: RequestContextC s sc m => DocumentTemplateFile -> m Int64
updateFileByUuid file = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "UPDATE %s SET document_template_uuid = ?, uuid = ?, file_name = ?, content = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [entityName]
  let params = toRow file ++ [toField tenantUuid, toField file.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFiles :: RequestContextC s sc m => m Int64
deleteFiles = do
  createDeleteEntitiesFn entityName

deleteFilesByDocumentTemplateUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteFilesByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

deleteFileById :: RequestContextC s sc m => U.UUID -> m Int64
deleteFileById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
