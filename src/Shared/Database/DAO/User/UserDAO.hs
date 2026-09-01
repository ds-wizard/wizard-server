module Shared.Database.DAO.User.UserDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.User.User ()
import Shared.Database.Mapping.User.UserSuggestion ()
import Shared.Database.Mapping.User.UserWithMembership ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.User
import Shared.Model.User.UserSuggestion
import Shared.Model.User.UserWithMembership
import Shared.Util.String

entityName = "user_entity"

pageLabel = "users"

findUsers :: WizardRequestContextC s m => m [User]
findUsers = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("machine", "false")]

findUsersFiltered :: WizardRequestContextC s m => [(String, String)] -> m [User]
findUsersFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  findUsersWithTenantFiltered tenantUuid queryParams

findUsersWithTenantFiltered :: WizardRequestContextC s m => U.UUID -> [(String, String)] -> m [User]
findUsersWithTenantFiltered tenantUuid queryParams =
  createFindEntitiesByFn entityName ([tenantQueryUuid tenantUuid, ("machine", "false")] ++ queryParams)

findUsersPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Pageable -> [Sort] -> m (Page User)
findUsersPage mQuery mRole pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND role_uuid::text ~* ? AND tenant_uuid = ? AND machine = false"
    [regexM mQuery, regexM mQuery, regexM mRole, U.toString tenantUuid]

findUserSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> m (Page UserSuggestion)
findUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let selectCondition =
        case mSelectUuids of
          Nothing -> ""
          Just [] -> ""
          Just selectUuids -> f' "AND uuid IN (%s)" [generateQuestionMarks selectUuids]
  let excludeCondition =
        case mExcludeUuids of
          Nothing -> ""
          Just [] -> ""
          Just excludeUuids -> f' "AND uuid NOT IN (%s)" [generateQuestionMarks excludeUuids]
  let condition =
        f'
          "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?) AND active = true AND tenant_uuid = ? AND machine = false %s %s"
          [selectCondition, excludeCondition]
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, first_name, last_name, gravatar_hash(email), image_url, affiliation"
    condition
    ([regexM mQuery, regexM mQuery, U.toString tenantUuid] ++ fromMaybe [] mSelectUuids ++ fromMaybe [] mExcludeUuids)

findUsersByEmails :: WizardRequestContextC s m => [String] -> m [User]
findUsersByEmails emails = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesInFn entityName tenantUuid "email" emails

findUsersByUserGroupUuid :: WizardRequestContextC s m => U.UUID -> m [UserWithMembership]
findUsersByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT u.uuid, u.first_name, u.last_name, u.email, u.image_url, u.affiliation, ugm.type \
          \FROM user_group_membership ugm \
          \JOIN user_entity u ON u.uuid = ugm.user_uuid AND u.tenant_uuid = ugm.tenant_uuid \
          \WHERE ugm.user_group_uuid = ? AND ugm.tenant_uuid = ? \
          \ORDER BY u.uuid"
  let params = [U.toString userGroupUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUserByUuid :: WizardRequestContextC s m => U.UUID -> m User
findUserByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]

findUserByUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe User)
findUserByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [("tenant_uuid", U.toString tenantUuid), ("uuid", U.toString uuid)]

findUserByUuidAndTenantUuidSystem :: WizardRequestContextC s m => U.UUID -> U.UUID -> m User
findUserByUuidAndTenantUuidSystem uuid tenantUuid = createFindEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]

findUserByUuidSystem' :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (Maybe User)
findUserByUuidSystem' uuid tenantUuid = createFindEntityByFn' entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]

findUserByEmail :: WizardRequestContextC s m => String -> m User
findUserByEmail email = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("email", email)]

findUserByEmail' :: WizardRequestContextC s m => String -> m (Maybe User)
findUserByEmail' email = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("email", email), ("machine", "false")]

findUserByEmailAndTenantUuid' :: WizardRequestContextC s m => String -> U.UUID -> m (Maybe User)
findUserByEmailAndTenantUuid' email tenantUuid = createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("email", email)]

countUsers :: WizardRequestContextC s m => m Int
countUsers = do
  tenantUuid <- asks (.tenantUuid')
  countUsersWithTenant tenantUuid

countUsersWithTenant :: WizardRequestContextC s m => U.UUID -> m Int
countUsersWithTenant tenantUuid = createCountByFn entityName (f' "%s AND machine = false" [tenantCondition]) [tenantUuid]

countUsersByRole :: WizardRequestContextC s m => U.UUID -> m Int
countUsersByRole roleUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT COUNT(*) FROM user_entity WHERE role_uuid = ? AND tenant_uuid = ?"
  let params = [U.toString roleUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [Only count] -> return count
    _ -> return 0

countActiveUsers :: WizardRequestContextC s m => m Int
countActiveUsers = do
  tenantUuid <- asks (.tenantUuid')
  countActiveUsersWithTenant tenantUuid

countActiveUsersWithTenant :: WizardRequestContextC s m => U.UUID -> m Int
countActiveUsersWithTenant tenantUuid = createCountByFn entityName (f' "%s AND machine = false AND active = true" [tenantCondition]) [U.toString tenantUuid]

insertUser :: WizardRequestContextC s m => User -> m Int64
insertUser user = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "INSERT INTO user_entity VALUES (?, ?, ?, ?, ?, ?, ?, ?::varchar[], ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  let params = toRow user
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserByUuid :: WizardRequestContextC s m => User -> m Int64
updateUserByUuid user = do
  let sql =
        fromString
          "UPDATE user_entity SET uuid = ?, first_name = ?, last_name = ?, email = ?, password_hash = ?, affiliation = ?, role_uuid = ?, role_permissions = ?, active = ?, image_url = ?, last_visited_at = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, machine = ?, locale = ?, last_seen_news_id = ?, email_verified_at = ?, email_pending = ?, role_name = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow user ++ [toField user.tenantUuid, toField user.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserPasswordByUuid :: WizardRequestContextC s m => U.UUID -> String -> UTCTime -> m Int64
updateUserPasswordByUuid userUuid uPassword uUpdatedAt = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE user_entity SET password_hash = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField uPassword, toField uUpdatedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserLastVisitedAtByUuid :: WizardRequestContextC s m => U.UUID -> UTCTime -> m Int64
updateUserLastVisitedAtByUuid userUuid lastVisitedAt = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE user_entity SET last_visited_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField lastVisitedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserLocaleByUuid :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> UTCTime -> m Int64
updateUserLocaleByUuid userUuid mLocale uUpdatedAt = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE user_entity SET locale = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField mLocale, toField uUpdatedAt, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUserLastSeenNewsIdUuid :: WizardRequestContextC s m => U.UUID -> String -> m Int64
updateUserLastSeenNewsIdUuid userUuid lastSeenNewsId = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE user_entity SET last_seen_news_id = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField lastSeenNewsId, toField tenantUuid, toField userUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateUsersRoleByRole :: WizardRequestContextC s m => U.UUID -> [String] -> String -> m Int64
updateUsersRoleByRole roleUuid permissions roleName = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE user_entity SET role_permissions = ?, role_name = ? WHERE role_uuid = ? AND tenant_uuid = ?"
  let params = (PGArray permissions, roleName, U.toString roleUuid, U.toString tenantUuid)
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUsers :: WizardRequestContextC s m => m Int64
deleteUsers = createDeleteEntitiesFn entityName

deleteUserByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteUserByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
