module Shared.Database.DAO.Workspace.WorkspaceMembershipDAO where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Workspace.WorkspaceMember ()
import Shared.Database.Mapping.Workspace.WorkspaceMembership ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.User.RoleSimple
import Shared.Model.Workspace.WorkspaceMember
import Shared.Model.Workspace.WorkspaceMembership

entityName = "workspace_membership"

pageLabel = "members"

findWorkspaceMembershipsByUserUuid :: RequestContextC s sc m => U.UUID -> m [WorkspaceMembership]
findWorkspaceMembershipsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findWorkspaceMembership :: RequestContextC s sc m => U.UUID -> U.UUID -> m WorkspaceMembership
findWorkspaceMembership workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("workspace_uuid", U.toString workspaceUuid), ("user_uuid", U.toString userUuid)]

findWorkspaceMembership' :: RequestContextC s sc m => U.UUID -> U.UUID -> m (Maybe WorkspaceMembership)
findWorkspaceMembership' workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("workspace_uuid", U.toString workspaceUuid), ("user_uuid", U.toString userUuid)]

findWorkspaceMembersPage :: RequestContextC s sc m => U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page WorkspaceMember)
findWorkspaceMembersPage workspaceUuid mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let withSelect =
        "SELECT u.uuid, u.first_name, u.last_name, u.email, u.image_url, wm.created_at, r.uuid AS role_uuid, r.name AS role_name, r.permissions AS role_permissions \
        \FROM workspace_membership wm \
        \JOIN user_entity u ON u.uuid = wm.user_uuid AND u.tenant_uuid = wm.tenant_uuid AND u.machine = false \
        \JOIN role r ON r.uuid = wm.role_uuid \
        \WHERE wm.tenant_uuid = ? AND wm.workspace_uuid = ?"
  let condition = "WHERE concat(first_name, ' ', last_name, ' ', email) ~* ?"
  createFindEntitiesPageableQuerySortWithComputedEntityFn withSelect pageLabel pageable sort "*" condition [U.toString tenantUuid, U.toString workspaceUuid, regexM mQuery]

findWorkspaceRolesByUserUuid :: RequestContextC s sc m => U.UUID -> m (M.Map U.UUID RoleSimple)
findWorkspaceRolesByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT wm.workspace_uuid, r.uuid, r.name, r.permissions \
          \FROM workspace_membership wm \
          \JOIN role r ON r.uuid = wm.role_uuid AND r.workspace_uuid = wm.workspace_uuid \
          \WHERE wm.tenant_uuid = ? AND wm.user_uuid = ?"
  let params = [toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = query conn sql params
  rows <- runDB action
  return . M.fromList $ fmap (\(workspaceUuid, roleUuid, roleName, PGArray permissions) -> (workspaceUuid, RoleSimple {uuid = roleUuid, name = roleName, permissions = permissions})) rows

countWorkspaceMembershipsByRole :: RequestContextC s sc m => U.UUID -> m Int
countWorkspaceMembershipsByRole roleUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT COUNT(*) FROM workspace_membership wm JOIN user_entity u ON u.uuid = wm.user_uuid AND u.machine = false WHERE wm.tenant_uuid = ? AND wm.role_uuid = ?"
  let params = [toField tenantUuid, toField roleUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [Only count] -> return count
    _ -> return 0

updateWorkspaceMembershipRole :: RequestContextC s sc m => U.UUID -> U.UUID -> U.UUID -> m Int64
updateWorkspaceMembershipRole workspaceUuid userUuid roleUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE workspace_membership SET role_uuid = ? WHERE tenant_uuid = ? AND workspace_uuid = ? AND user_uuid = ?"
  let params = [toField roleUuid, toField tenantUuid, toField workspaceUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

insertWorkspaceMembership :: RequestContextC s sc m => WorkspaceMembership -> m Int64
insertWorkspaceMembership membership = do
  let sql = fromString "INSERT INTO workspace_membership VALUES (?, ?, ?, ?, ?) ON CONFLICT (workspace_uuid, user_uuid) DO UPDATE SET role_uuid = EXCLUDED.role_uuid"
  let params = toRow membership
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteWorkspaceMembership :: RequestContextC s sc m => U.UUID -> U.UUID -> m Int64
deleteWorkspaceMembership workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("workspace_uuid", U.toString workspaceUuid), ("user_uuid", U.toString userUuid)]

findUserGroupUuidsInWorkspaceByUserUuid :: RequestContextC s sc m => U.UUID -> U.UUID -> m [U.UUID]
findUserGroupUuidsInWorkspaceByUserUuid workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT user_group_uuid FROM user_group_membership \
          \WHERE tenant_uuid = ? AND user_uuid = ? \
          \  AND user_group_uuid IN (SELECT uuid FROM user_group WHERE tenant_uuid = ? AND workspace_uuid = ?)"
  let params = [toField tenantUuid, toField userUuid, toField tenantUuid, toField workspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  fmap (fmap fromOnly) (runDB action)

deleteUserGroupMembershipsInWorkspaceByUserUuid :: RequestContextC s sc m => U.UUID -> U.UUID -> m Int64
deleteUserGroupMembershipsInWorkspaceByUserUuid workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM user_group_membership \
          \WHERE tenant_uuid = ? AND user_uuid = ? \
          \  AND user_group_uuid IN (SELECT uuid FROM user_group WHERE tenant_uuid = ? AND workspace_uuid = ?)"
  let params = [toField tenantUuid, toField userUuid, toField tenantUuid, toField workspaceUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectPermsInWorkspaceByUserUuid :: RequestContextC s sc m => U.UUID -> U.UUID -> m Int64
deleteProjectPermsInWorkspaceByUserUuid workspaceUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM project_perm_user \
          \WHERE tenant_uuid = ? AND user_uuid = ? \
          \  AND project_uuid IN (SELECT uuid FROM project WHERE tenant_uuid = ? AND workspace_uuid = ?)"
  let params = [toField tenantUuid, toField userUuid, toField tenantUuid, toField workspaceUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
