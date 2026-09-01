module Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateAsset ()
import Shared.Model.Context.RequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.String

entityName = "document_template_asset"

findAssetsByDocumentTemplateUuid :: RequestContextC s sc m => U.UUID -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findAssetsByDocumentTemplateUuidAndFileName :: RequestContextC s sc m => U.UUID -> String -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateUuidAndFileName documentTemplateUuid fileName = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("file_name", fileName)]

findAssetById :: RequestContextC s sc m => U.UUID -> m DocumentTemplateAsset
findAssetById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

sumAssetFileSize :: RequestContextC s sc m => m Int64
sumAssetFileSize = do
  tenantUuid <- asks (.tenantUuid')
  sumAssetFileSizeWithTenant tenantUuid

sumAssetFileSizeWithTenant :: RequestContextC s sc m => U.UUID -> m Int64
sumAssetFileSizeWithTenant tenantUuid = do
  createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertAsset :: RequestContextC s sc m => DocumentTemplateAsset -> m Int64
insertAsset asset = do
  createInsertFn entityName asset

updateAssetById :: RequestContextC s sc m => DocumentTemplateAsset -> m Int64
updateAssetById asset = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "UPDATE %s SET document_template_uuid = ?, uuid = ?, file_name = ?, content_type = ?, tenant_uuid = ?, file_size = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [entityName]
  let params = toRow asset ++ [toField tenantUuid, toField asset.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAssets :: RequestContextC s sc m => m Int64
deleteAssets = do
  createDeleteEntitiesFn entityName

deleteAssetsByDocumentTemplateUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteAssetsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

deleteAssetById :: RequestContextC s sc m => U.UUID -> m Int64
deleteAssetById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
