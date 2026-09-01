module WizardLib.Public.Database.DAO.User.RoleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.User.Role ()
import WizardLib.Public.Database.Mapping.User.RoleList ()
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList

entityName = "role"

pageLabel = "roles"

roleListFields :: AppContextC s sc m => m String
roleListFields = do
  table <- tableName entityName
  userTable <- tableName "user_entity"
  return $
    f''
      "uuid, name, permissions, (SELECT count(*) FROM ${user} WHERE role_uuid = ${role}.uuid AND tenant_uuid = ${role}.tenant_uuid) AS users_count, is_admin"
      [("user", userTable), ("role", table)]

findRoles :: AppContextC s sc m => m [Role]
findRoles = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid]

findRolesPage :: AppContextC s sc m => Maybe String -> Pageable -> [Sort] -> m (Page RoleList)
findRolesPage mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  fields <- roleListFields
  createFindEntitiesPageableQuerySortFn
    table
    pageLabel
    pageable
    sort
    fields
    "WHERE name ~* ? AND tenant_uuid = ?"
    [regexM mQuery, U.toString tenantUuid]

findRoleListByUuid :: AppContextC s sc m => U.UUID -> m RoleList
findRoleListByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  fields <- roleListFields
  createFindEntityWithFieldsByFn
    fields
    False
    table
    [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid :: AppContextC s sc m => U.UUID -> m Role
findRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  findRoleByUuidAndTenant uuid tenantUuid

findRoleByUuidAndTenant :: AppContextC s sc m => U.UUID -> U.UUID -> m Role
findRoleByUuidAndTenant uuid tenantUuid = do
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findRoleByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe Role)
findRoleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertRole :: AppContextC s sc m => Role -> m Int64
insertRole role = do
  table <- tableName entityName
  createInsertFn table role

updateRoleByUuid :: AppContextC s sc m => Role -> m Int64
updateRoleByUuid role = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, permissions = ?, is_admin = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [table]
  let params = toRow role ++ [toField tenantUuid, toField role.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteRoles :: AppContextC s sc m => m Int64
deleteRoles = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteRoleByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteRoleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
