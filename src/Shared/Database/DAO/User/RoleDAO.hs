module Shared.Database.DAO.User.RoleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.Role ()
import Shared.Database.Mapping.User.RoleList ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Util.String

entityName = "role"

pageLabel = "roles"

roleListFields :: RequestContextC s sc m => m String
roleListFields = do
  return $
    f''
      "uuid, name, permissions, CASE WHEN ${role}.workspace_uuid IS NULL THEN (SELECT count(*) FROM ${user} WHERE role_uuid = ${role}.uuid AND tenant_uuid = ${role}.tenant_uuid) ELSE (SELECT count(*) FROM workspace_membership wm JOIN ${user} u ON u.uuid = wm.user_uuid AND u.machine = false WHERE wm.role_uuid = ${role}.uuid) END AS users_count, is_admin, workspace_uuid"
      [("user", "user_entity"), ("role", entityName)]

findRoles :: RequestContextC s sc m => m [Role]
findRoles = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findRolesPage :: RequestContextC s sc m => Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page RoleList)
findRolesPage mWorkspaceUuid mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  fields <- roleListFields
  let roleWorkspaceCondition = maybe "workspace_uuid IS NULL" (\workspaceUuid -> f' "workspace_uuid = '%s'" [U.toString workspaceUuid]) mWorkspaceUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    fields
    (f' "WHERE name ~* ? AND tenant_uuid = ? AND %s" [roleWorkspaceCondition])
    [regexM mQuery, U.toString tenantUuid]

findRoleListByUuid :: RequestContextC s sc m => U.UUID -> m RoleList
findRoleListByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  fields <- roleListFields
  createFindEntityWithFieldsByFn
    fields
    False
    entityName
    [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid :: RequestContextC s sc m => U.UUID -> m Role
findRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  findRoleByUuidAndTenant uuid tenantUuid

findRoleByUuidAndTenant :: RequestContextC s sc m => U.UUID -> U.UUID -> m Role
findRoleByUuidAndTenant uuid tenantUuid = do
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe Role)
findRoleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertRole :: RequestContextC s sc m => Role -> m Int64
insertRole role = do
  createInsertFn entityName role

updateRoleByUuid :: RequestContextC s sc m => Role -> m Int64
updateRoleByUuid role = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, permissions = ?, is_admin = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, workspace_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
            [entityName]
  let params = toRow role ++ [toField tenantUuid, toField role.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteRoles :: RequestContextC s sc m => m Int64
deleteRoles = do
  createDeleteEntitiesFn entityName

deleteRoleByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
