module Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssets, deleteAssetsByDocumentTemplateUuid)
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFiles, deleteFilesByDocumentTemplateUuid)
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.String

entityName = "document_template"

findDocumentTemplates :: RequestContextC s sc m => m [DocumentTemplate]
findDocumentTemplates = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM %s \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
            [entityName]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesFiltered :: RequestContextC s sc m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findDocumentTemplatesById :: RequestContextC s sc m => String -> Maybe U.UUID -> m [DocumentTemplate]
findDocumentTemplatesById dtId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND id = ? AND %s" [entityName, workspaceVisibleCondition]
  let params = [toField tenantUuid, toField dtId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesByIdInWorkspace :: RequestContextC s sc m => String -> Maybe U.UUID -> m [DocumentTemplate]
findDocumentTemplatesByIdInWorkspace dtId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND id = ? AND %s" [entityName, workspaceExactCondition]
  let params = [toField tenantUuid, toField dtId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesInWorkspace :: RequestContextC s sc m => Maybe U.UUID -> m [DocumentTemplate]
findDocumentTemplatesInWorkspace mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND %s" [entityName, workspaceVisibleCondition]
  let params = [toField tenantUuid, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplateByUuid :: RequestContextC s sc m => U.UUID -> m DocumentTemplate
findDocumentTemplateByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findDocumentTemplateByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe DocumentTemplate)
findDocumentTemplateByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findDocumentTemplateByCoordinate :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m DocumentTemplate
findDocumentTemplateByCoordinate coordinate mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  action <- createFindDocumentTemplateByCoordinateAction coordinate mWorkspaceUuid
  runOneEntityDB entityName action [tenantQueryUuid tenantUuid, ("id", coordinate.id), ("version", coordinate.version)]

findDocumentTemplateByCoordinate' :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m (Maybe DocumentTemplate)
findDocumentTemplateByCoordinate' coordinate mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  action <- createFindDocumentTemplateByCoordinateAction coordinate mWorkspaceUuid
  runOneEntityDB' entityName action [tenantQueryUuid tenantUuid, ("id", coordinate.id), ("version", coordinate.version)]

createFindDocumentTemplateByCoordinateAction :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m (Connection -> IO [DocumentTemplate])
createFindDocumentTemplateByCoordinateAction coordinate mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT * FROM %s WHERE tenant_uuid = ? AND id = ? AND version = ? AND %s ORDER BY workspace_uuid IS NOT NULL DESC LIMIT 1"
            [entityName, workspaceVisibleCondition]
  let params = [toField tenantUuid, toField coordinate.id, toField coordinate.version, toField mWorkspaceUuid]
  logQuery sql params
  return (\conn -> query conn sql params)

findLatestDocumentTemplateById :: RequestContextC s sc m => String -> Maybe U.UUID -> m DocumentTemplate
findLatestDocumentTemplateById dtId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM %s \
            \WHERE tenant_uuid = ? \
            \  AND id = ? \
            \  AND %s \
            \ORDER BY split_part(version, '.', 1)::int DESC, \
            \        split_part(version, '.', 2)::int DESC, \
            \        split_part(version, '.', 3)::int DESC, \
            \        workspace_uuid IS NOT NULL DESC \
            \LIMIT 1"
            [entityName, workspaceVisibleCondition]
  let params = [toField tenantUuid, toField dtId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB entityName action [("id", dtId)]

countDocumentTemplatesGroupedById :: RequestContextC s sc m => m Int
countDocumentTemplatesGroupedById = do
  tenantUuid <- asks (.tenantUuid')
  countDocumentTemplatesGroupedByIdWithTenant tenantUuid

countDocumentTemplatesGroupedByIdWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countDocumentTemplatesGroupedByIdWithTenant tenantUuid = do
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM %s \
            \      WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') \
            \      GROUP BY id) nested;"
            [entityName]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertDocumentTemplate :: RequestContextC s sc m => DocumentTemplate -> m Int64
insertDocumentTemplate documentTemplate = do
  createInsertFn entityName documentTemplate

updateDocumentTemplateById :: RequestContextC s sc m => DocumentTemplate -> m Int64
updateDocumentTemplateById dt = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, created_at = ?, tenant_uuid = ?, updated_at = ?, phase = ?, non_editable = ?, language = ?, workspace_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
            [entityName]
  let params = init (init (toRow dt)) ++ [toField dt.workspaceUuid, toField tenantUuid, toField dt.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateDocumentTemplatePotFileReadyByUuid :: RequestContextC s sc m => U.UUID -> Bool -> m Int64
updateDocumentTemplatePotFileReadyByUuid uuid potFileReady = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "UPDATE %s SET pot_file_ready = ? WHERE tenant_uuid = ? AND uuid = ?" [entityName]
  let params = [toField potFileReady, toField tenantUuid, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplates :: RequestContextC s sc m => m Int64
deleteDocumentTemplates = do
  deleteFiles
  deleteAssets
  createDeleteEntitiesFn entityName

deleteDocumentTemplatesFiltered :: RequestContextC s sc m => [(String, String)] -> m Int64
deleteDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
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
            [entityName, queryCondition]
  let params = U.toString tenantUuid : fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteDocumentTemplateByUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  deleteFilesByDocumentTemplateUuid documentTemplateUuid
  deleteAssetsByDocumentTemplateUuid documentTemplateUuid
  let sql =
        fromString $
          f'
            "DELETE FROM %s \
            \WHERE uuid = ? AND tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
            [entityName]
  let params = [U.toString documentTemplateUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
