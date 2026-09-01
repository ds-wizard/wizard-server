module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssets, deleteAssetsByDocumentTemplateUuid)
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFiles, deleteFilesByDocumentTemplateUuid)
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

findDocumentTemplates :: AppContextC s sc m => m [DocumentTemplate]
findDocumentTemplates = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM %s \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
            [table]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesFiltered :: AppContextC s sc m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table (tenantQueryUuid tenantUuid : queryParams)

findDocumentTemplatesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [DocumentTemplate]
findDocumentTemplatesByOrganizationIdAndKmId organizationId templateId = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn
    table
    [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("template_id", templateId)]

findDocumentTemplateByUuid :: AppContextC s sc m => U.UUID -> m DocumentTemplate
findDocumentTemplateByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findDocumentTemplateByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe DocumentTemplate)
findDocumentTemplateByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findDocumentTemplateByCoordinate :: AppContextC s sc m => Coordinate -> m DocumentTemplate
findDocumentTemplateByCoordinate coordinate = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("organization_id", coordinate.organizationId), ("template_id", coordinate.entityId), ("version", coordinate.version)]

findDocumentTemplateByCoordinate' :: AppContextC s sc m => Coordinate -> m (Maybe DocumentTemplate)
findDocumentTemplateByCoordinate' coordinate = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("organization_id", coordinate.organizationId), ("template_id", coordinate.entityId), ("version", coordinate.version)]

findLatestDocumentTemplateByOrganizationIdAndTemplateId :: AppContextC s sc m => String -> String -> m DocumentTemplate
findLatestDocumentTemplateByOrganizationIdAndTemplateId orgId templateId = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM %s \
            \WHERE tenant_uuid = ? \
            \  AND organization_id = ? \
            \  AND template_id = ? \
            \ORDER BY split_part(version, '.', 1)::int DESC, \
            \        split_part(version, '.', 2)::int DESC, \
            \        split_part(version, '.', 3)::int DESC \
            \LIMIT 1"
            [table]
  let params = [U.toString tenantUuid, orgId, templateId]
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB table action [("organization_id", orgId), ("template_id", templateId)]

countDocumentTemplatesGroupedByOrganizationIdAndKmId :: AppContextC s sc m => m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmId = do
  tenantUuid <- asks (.tenantUuid')
  countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid

countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant :: AppContextC s sc m => U.UUID -> m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid = do
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM %s \
            \      WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') \
            \      GROUP BY organization_id, template_id) nested;"
            [table]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertDocumentTemplate :: AppContextC s sc m => DocumentTemplate -> m Int64
insertDocumentTemplate documentTemplate = do
  table <- tableName entityName
  createInsertFn table documentTemplate

updateDocumentTemplateById :: AppContextC s sc m => DocumentTemplate -> m Int64
updateDocumentTemplateById dt = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, organization_id = ?, template_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, created_at = ?, tenant_uuid = ?, updated_at = ?, phase = ?, non_editable = ? WHERE tenant_uuid = ? AND uuid = ?"
            [table]
  let params = toRow dt ++ [toField tenantUuid, toField dt.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplates :: AppContextC s sc m => m Int64
deleteDocumentTemplates = do
  table <- tableName entityName
  deleteFiles
  deleteAssets
  createDeleteEntitiesFn table

deleteDocumentTemplatesFiltered :: AppContextC s sc m => [(String, String)] -> m Int64
deleteDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  templates <- findDocumentTemplatesFiltered queryParams
  traverse_ (\t -> deleteFilesByDocumentTemplateUuid t.uuid) templates
  traverse_ (\t -> deleteAssetsByDocumentTemplateUuid t.uuid) templates
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "DELETE FROM %s \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [table, queryCondition]
  let params = U.toString tenantUuid : fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteDocumentTemplateByUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  deleteFilesByDocumentTemplateUuid documentTemplateUuid
  deleteAssetsByDocumentTemplateUuid documentTemplateUuid
  let sql =
        fromString $
          f'
            "DELETE FROM %s \
            \WHERE uuid = ? AND tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
            [table]
  let params = [U.toString documentTemplateUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
