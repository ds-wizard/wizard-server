module Shared.Database.DAO.User.UserGroupMembershipDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.UserGroupMembership ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserGroupMembership
import Shared.Util.String

entityName = "user_group_membership"

findUserGroupMembershipsByUserUuid :: RequestContextC s sc m => U.UUID -> m [UserGroupMembership]
findUserGroupMembershipsByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserGroupMembershipsByUserGroupUuid :: RequestContextC s sc m => U.UUID -> m [UserGroupMembership]
findUserGroupMembershipsByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)] [Sort "user_uuid" Ascending]

findUserGroupMembershipByUserGroupUuidAndUserUuid' :: RequestContextC s sc m => U.UUID -> U.UUID -> m (Maybe UserGroupMembership)
findUserGroupMembershipByUserGroupUuidAndUserUuid' userGroupUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid), ("user_uuid", U.toString userUuid)]

insertUserGroupMembership :: RequestContextC s sc m => UserGroupMembership -> m Int64
insertUserGroupMembership userGroupMembership = do
  createInsertFn entityName userGroupMembership

updateUserGroupMembershipByUuid :: RequestContextC s sc m => UserGroupMembership -> m Int64
updateUserGroupMembershipByUuid userGroupMembership = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET user_group_uuid = ?, user_uuid = ?, type = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE user_group_uuid = ? AND user_uuid = ? AND tenant_uuid = ?"
            [entityName]
  let params = toRow userGroupMembership ++ [toField userGroupMembership.userGroupUuid, toField userGroupMembership.userUuid, toField userGroupMembership.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserGroupMemberships :: RequestContextC s sc m => m Int64
deleteUserGroupMemberships = do
  createDeleteEntitiesFn entityName

deleteUserGroupMembershipsByUserGroupUuid :: RequestContextC s sc m => U.UUID -> m ()
deleteUserGroupMembershipsByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)]
  return ()

deleteUserGroupMembershipsByUserGroupUuidAndUserUuids :: RequestContextC s sc m => U.UUID -> [U.UUID] -> m ()
deleteUserGroupMembershipsByUserGroupUuidAndUserUuids userGroupUuid userUuids = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM %s WHERE tenant_uuid = ? AND user_group_uuid = ? AND user_uuid IN (%s)"
            [entityName, generateQuestionMarks userUuids]
  let params = fmap toField (tenantUuid : userGroupUuid : userUuids)
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return ()
