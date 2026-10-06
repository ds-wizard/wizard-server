module Shared.Database.DAO.User.UserGroupDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.User.UserGroup ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserGroup
import Shared.Util.String

entityName = "user_group"

pageLabel = "userGroups"

createFindUserGroupPage :: (WizardRequestContextC s m, FromRow userGroup) => String -> U.UUID -> String -> Maybe String -> String -> Pageable -> [Sort] -> m (Page userGroup)
createFindUserGroupPage fields currentUserUuid manageCondition mQuery additionalCondition pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    workspaceCondition <- workspaceOnlyCondition Nothing "ug.workspace_uuid"
    let membershipTable = "user_group_membership"
    let (nameCondition, nameRegex) =
          case mQuery of
            Just query -> (" AND ug.name ~* ?", [regex query])
            Nothing -> ("", [])
    let aclJoins = f' "LEFT JOIN %s ugm ON ugm.user_group_uuid = ug.uuid AND ugm.tenant_uuid = ug.tenant_uuid" [membershipTable]
    let aclCondition = f' "AND (ug.private IS FALSE OR ugm.user_uuid = '%s' OR %s)" [U.toString currentUserUuid, manageCondition]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT COUNT(DISTINCT ug.uuid) \
              \FROM ${userGroup} ug \
              \${aclJoin} \
              \WHERE ug.tenant_uuid = '${tenantUuid}' ${workspaceCondition} ${nameCondition} ${aclCondition} ${additionalCondition}"
              [ ("userGroup", entityName)
              , ("userUuid", U.toString currentUserUuid)
              , ("aclJoin", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("workspaceCondition", workspaceCondition)
              , ("nameCondition", nameCondition)
              , ("aclCondition", aclCondition)
              , ("additionalCondition", additionalCondition)
              ]
    let params = nameRegex
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT DISTINCT ${fields} \
              \FROM ${userGroup} ug \
              \${aclJoin} \
              \WHERE ug.tenant_uuid = '${tenantUuid}' ${workspaceCondition} ${nameCondition} ${aclCondition} ${additionalCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("fields", fields)
              , ("userGroup", entityName)
              , ("userUuid", U.toString currentUserUuid)
              , ("aclJoin", aclJoins)
              , ("tenantUuid", U.toString tenantUuid)
              , ("workspaceCondition", workspaceCondition)
              , ("nameCondition", nameCondition)
              , ("aclCondition", aclCondition)
              , ("additionalCondition", additionalCondition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findUserGroupByUuid :: WizardRequestContextC s m => U.UUID -> m UserGroup
findUserGroupByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertUserGroup :: WizardRequestContextC s m => UserGroup -> m Int64
insertUserGroup userGroup = do
  createInsertFn entityName userGroup

updateUserGroupByUuid :: WizardRequestContextC s m => UserGroup -> m Int64
updateUserGroupByUuid userGroup = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, description = ?, private = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, workspace_uuid = ? WHERE uuid = ? AND tenant_uuid = ?"
            [entityName]
  let params = toRow userGroup ++ [toField userGroup.uuid, toField userGroup.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserGroups :: WizardRequestContextC s m => m Int64
deleteUserGroups = do
  createDeleteEntitiesFn entityName

deleteUserGroupByUuid :: WizardRequestContextC s m => U.UUID -> m ()
deleteUserGroupByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
