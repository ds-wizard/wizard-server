module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFile ()
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFileList ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

entityName = "document_template_file"

findFilesByDocumentTemplateUuid :: AppContextC s sc m => U.UUID -> m [DocumentTemplateFile]
findFilesByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findFileListsByDocumentTemplateUuid :: AppContextC s sc m => U.UUID -> m [DocumentTemplateFileList]
findFileListsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesWithFieldsByFn "uuid, file_name, created_at, updated_at" table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findFilesByDocumentTemplateUuidAndFileName :: AppContextC s sc m => U.UUID -> String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateUuidAndFileName documentTemplateUuid fileName = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("file_name", fileName)]

findFileByUuid :: AppContextC s sc m => U.UUID -> m DocumentTemplateFile
findFileByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertFile :: AppContextC s sc m => DocumentTemplateFile -> m Int64
insertFile file = do
  table <- tableName entityName
  createInsertFn table file

updateFileByUuid :: AppContextC s sc m => DocumentTemplateFile -> m Int64
updateFileByUuid file = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET document_template_uuid = ?, uuid = ?, file_name = ?, content = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [table]
  let params = toRow file ++ [toField tenantUuid, toField file.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFiles :: AppContextC s sc m => m Int64
deleteFiles = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteFilesByDocumentTemplateUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteFilesByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntitiesByFn table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

deleteFileById :: AppContextC s sc m => U.UUID -> m Int64
deleteFileById uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
