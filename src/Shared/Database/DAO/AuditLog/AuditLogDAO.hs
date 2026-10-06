module Shared.Database.DAO.AuditLog.AuditLogDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.AuditLog.AuditLog ()
import Shared.Model.AuditLog.AuditLog
import Shared.Model.AuditLog.AuditLogFilter
import Shared.Model.AuditLog.AuditLogSource
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Util.String

entityName = "audit_log"

pageLabel = "auditLogs"

findAuditLogsPage :: RequestContextC s sc m => AuditLogFilter -> Pageable -> [Sort] -> m (Page AuditLog)
findAuditLogsPage auditLogFilter pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let conditions =
        [("tenant_uuid = ?", [toField tenantUuid])]
          ++ queryCondition auditLogFilter.query
          ++ inCondition "component" auditLogFilter.components
          ++ inCondition "action" auditLogFilter.actions
          ++ valueCondition "created_by_uuid = ?" auditLogFilter.createdBy
          ++ valueCondition "source_type = ?" (fmap auditLogSourceTypeName auditLogFilter.source)
          ++ valueCondition "created_at >= ?" auditLogFilter.from
          ++ valueCondition "created_at <= ?" auditLogFilter.to
          ++ workspaceCondition auditLogFilter.workspace
  let condition = "WHERE " ++ L.intercalate " AND " (fmap fst conditions)
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition (concatMap snd conditions)

findAuditLogs :: RequestContextC s sc m => m [AuditLog]
findAuditLogs = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "createdAt" Ascending]

findAuditLogByUuid :: RequestContextC s sc m => U.UUID -> m AuditLog
findAuditLogByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertAuditLog :: RequestContextC s sc m => AuditLog -> m Int64
insertAuditLog = createInsertFn entityName

deleteAuditLogByTenantUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteAuditLogByTenantUuid tenantUuid = createDeleteEntityByFn entityName [("tenant_uuid", U.toString tenantUuid)]

deleteAuditLogs :: RequestContextC s sc m => m Int64
deleteAuditLogs = createDeleteEntitiesFn entityName

queryCondition :: Maybe String -> [(String, [Action])]
queryCondition = valueCondition "action_data::text ~* ?"

inCondition :: String -> Maybe [String] -> [(String, [Action])]
inCondition _ Nothing = []
inCondition _ (Just []) = []
inCondition column (Just values) = [(f' "%s IN (%s)" [column, generateQuestionMarks values], fmap toField values)]

valueCondition :: ToField a => String -> Maybe a -> [(String, [Action])]
valueCondition condition = maybe [] (\value -> [(condition, [toField value])])

workspaceCondition :: AuditLogWorkspaceFilter -> [(String, [Action])]
workspaceCondition AllAuditLogWorkspaces = []
workspaceCondition OrganizationAuditLogWorkspaces = [("workspace_uuid IS NULL", [])]
workspaceCondition (SingleAuditLogWorkspace workspaceUuid) = [("workspace_uuid = ?", [toField workspaceUuid])]
