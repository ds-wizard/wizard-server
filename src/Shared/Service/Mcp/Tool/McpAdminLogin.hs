module Shared.Service.Mcp.Tool.McpAdminLogin where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Constant.User
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RoleSimple
import Shared.Model.User.User

checkAdminLoginRemains :: WizardRequestContextC s m => (U.UUID -> m [U.UUID]) -> U.UUID -> m ()
checkAdminLoginRemains findUserLoginProviders removedProviderUuid = checkSomeAdminCanLogIn findUserLoginProviders (const True) (/= removedProviderUuid)

checkAdminLoginRemainsForUser :: WizardRequestContextC s m => (U.UUID -> m [U.UUID]) -> U.UUID -> (Bool, U.UUID) -> (Bool, U.UUID) -> m ()
checkAdminLoginRemainsForUser findUserLoginProviders userUuid current changed = do
  adminRoleUuids <- findAdminRoleUuids
  let isActiveAdmin (active, roleUuid) = active && roleUuid `elem` adminRoleUuids
  when (isActiveAdmin current && not (isActiveAdmin changed)) $
    checkSomeAdminCanLogIn findUserLoginProviders ((/= userUuid) . (.uuid)) (const True)

checkSomeAdminCanLogIn :: WizardRequestContextC s m => (U.UUID -> m [U.UUID]) -> (User -> Bool) -> (U.UUID -> Bool) -> m ()
checkSomeAdminCanLogIn findUserLoginProviders staysAdmin keepsProvider = do
  adminRoleUuids <- findAdminRoleUuids
  admins <- filter (\user -> user.role.uuid `elem` adminRoleUuids && staysAdmin user) <$> findUsersFiltered [("active", "true")]
  logins <- traverse (canAdminLogIn findUserLoginProviders keepsProvider) admins
  unless (or logins) (throwError . UserError $ _ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN)

canAdminLogIn :: WizardRequestContextC s m => (U.UUID -> m [U.UUID]) -> (U.UUID -> Bool) -> User -> m Bool
canAdminLogIn findUserLoginProviders keepsProvider admin
  | admin.passwordHash /= noPasswordHash = return True
  | otherwise = any keepsProvider <$> findUserLoginProviders admin.uuid

findAdminRoleUuids :: WizardRequestContextC s m => m [U.UUID]
findAdminRoleUuids = fmap (.uuid) . filter (.isAdmin) <$> findRoles
