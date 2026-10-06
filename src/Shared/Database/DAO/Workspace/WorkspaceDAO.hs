module Shared.Database.DAO.Workspace.WorkspaceDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Workspace.Workspace ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.Workspace.Workspace
import Shared.Util.String

entityName = "workspace"

pageLabel = "workspaces"

findWorkspaces :: RequestContextC s sc m => m [Workspace]
findWorkspaces = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "name" Ascending]

findWorkspacesPage :: RequestContextC s sc m => String -> Maybe String -> Pageable -> [Sort] -> m (Page Workspace)
findWorkspacesPage accessCondition mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let condition = f' "WHERE tenant_uuid = ? AND name ~* ?%s" [accessCondition]
  createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort "*" condition [U.toString tenantUuid, regexM mQuery]

findWorkspaceByUuid :: RequestContextC s sc m => U.UUID -> m Workspace
findWorkspaceByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findWorkspaceByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe Workspace)
findWorkspaceByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findWorkspacesByTenantUuid :: RequestContextC s sc m => U.UUID -> m [Workspace]
findWorkspacesByTenantUuid tenantUuid = createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "created_at" Ascending]

findWorkspaceUuids :: RequestContextC s sc m => m [U.UUID]
findWorkspaceUuids = do
  tenantUuid <- asks (.tenantUuid')
  findWorkspaceUuidsByTenantUuid tenantUuid

findWorkspaceUuidsByTenantUuid :: RequestContextC s sc m => U.UUID -> m [U.UUID]
findWorkspaceUuidsByTenantUuid tenantUuid = do
  let sql = fromString "SELECT uuid FROM workspace WHERE tenant_uuid = ? ORDER BY created_at"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  fmap (fmap fromOnly) (runDB action)

findWorkspaceUuidsForUpdate :: RequestContextC s sc m => m [U.UUID]
findWorkspaceUuidsForUpdate = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT uuid FROM workspace WHERE tenant_uuid = ? ORDER BY created_at FOR UPDATE"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  fmap (fmap fromOnly) (runDB action)

insertWorkspace :: RequestContextC s sc m => Workspace -> m Int64
insertWorkspace = createInsertFn entityName

updateWorkspaceByUuid :: RequestContextC s sc m => Workspace -> m Int64
updateWorkspaceByUuid workspace = do
  let sql =
        fromString
          "UPDATE workspace SET uuid = ?, tenant_uuid = ?, name = ?, description = ?, created_at = ?, updated_at = ?, default_role_uuid = ?, logo = ?, primary_color = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow workspace ++ [toField workspace.uuid, toField workspace.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteWorkspaces :: RequestContextC s sc m => m Int64
deleteWorkspaces = createDeleteEntitiesFn entityName

deleteWorkspaceByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteWorkspaceByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
