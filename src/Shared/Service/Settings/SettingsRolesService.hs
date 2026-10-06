module Shared.Service.Settings.SettingsRolesService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Settings.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.User.Role
import Shared.Model.Workspace.Workspace
import Shared.Service.Settings.SettingsService
import Shared.Service.User.RoleAcl

getSettingsRoles :: WizardRequestContextC s m => m (SettingsDTO SettingsRoles)
getSettingsRoles = do
  mWorkspaceUuid <- requireSettingsScope
  checkScopedPermission _ROLES_MANAGE_ROLE_PERMISSION _ROLES_MANAGE_ROLE_PERMISSION mWorkspaceUuid
  value <- findSettingsRolesValue mWorkspaceUuid
  return $ SettingsDTO value Nothing Nothing

modifySettingsRoles :: WizardRequestContextC s m => SettingsDTO SettingsRoles -> m (SettingsDTO SettingsRoles)
modifySettingsRoles reqDto =
  runInTransaction $ do
    mWorkspaceUuid <- requireSettingsScope
    checkScopedPermission _ROLES_MANAGE_ROLE_PERMISSION _ROLES_MANAGE_ROLE_PERMISSION mWorkspaceUuid
    role <- findRoleByUuid reqDto.value.defaultRoleUuid
    saveSettingsRolesValue mWorkspaceUuid role
    return $ SettingsDTO reqDto.value Nothing Nothing

findSettingsRolesValue :: WizardRequestContextC s m => Maybe U.UUID -> m SettingsRoles
findSettingsRolesValue Nothing = getCurrentSettings findSettingsRoles
findSettingsRolesValue (Just workspaceUuid) = do
  workspace <- findWorkspaceByUuid workspaceUuid
  maybe (throwError . NotExistsError $ _ERROR_VALIDATION__ABSENCE "role") (return . SettingsRoles) workspace.defaultRoleUuid

saveSettingsRolesValue :: WizardRequestContextC s m => Maybe U.UUID -> Role -> m ()
saveSettingsRolesValue Nothing role = do
  checkOrganizationRole role
  tenantUuid <- asks (.tenantUuid')
  saveSettingsRoles tenantUuid (SettingsRoles role.uuid)
saveSettingsRolesValue (Just workspaceUuid) role = do
  unless (role.workspaceUuid == Just workspaceUuid) (throwError . UserError $ _ERROR_SERVICE_SETTINGS__ROLE_NOT_IN_WORKSPACE)
  workspace <- findWorkspaceByUuid workspaceUuid
  now <- liftIO getCurrentTime
  _ <- updateWorkspaceByUuid (workspace {defaultRoleUuid = Just role.uuid, updatedAt = now} :: Workspace)
  return ()
