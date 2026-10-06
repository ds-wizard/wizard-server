module Shared.Service.User.RoleAcl where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Service.Workspace.WorkspaceScopeService

checkRoleReadPermission :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkRoleReadPermission mWorkspaceUuid = do
  checkPermissionsAny [_ROLES_MANAGE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION, _MEMBERS_MANAGE_ROLE_PERMISSION]
  let peoplePermission = maybe _USERS_MANAGE_ROLE_PERMISSION (const _MEMBERS_MANAGE_ROLE_PERMISSION) mWorkspaceUuid
  hasRoles <- hasPermissionInWorkspace _ROLES_MANAGE_ROLE_PERMISSION mWorkspaceUuid
  hasPeople <- hasPermissionInWorkspace peoplePermission mWorkspaceUuid
  unless
    (hasRoles || hasPeople)
    (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission (need any): " ++ show [_ROLES_MANAGE_ROLE_PERMISSION, peoplePermission]))

checkOrganizationRole :: WizardRequestContextC s m => Role -> m ()
checkOrganizationRole role = when (isJust role.workspaceUuid) (throwError . UserError $ _ERROR_VALIDATION__USER_ROLE_NOT_ORGANIZATION)

checkRoleManagePermission :: WizardRequestContextC s m => Maybe U.UUID -> m ()
checkRoleManagePermission mWorkspaceUuid = do
  checkPermission _ROLES_MANAGE_ROLE_PERMISSION
  case mWorkspaceUuid of
    Just workspaceUuid -> do
      checkWorkspacePlaneAvailable
      checkPermissionInWorkspace _ROLES_MANAGE_ROLE_PERMISSION (Just workspaceUuid)
    Nothing -> do
      currentUser <- getCurrentUser
      unless
        (_ROLES_MANAGE_ROLE_PERMISSION `elem` currentUser.role.permissions)
        (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ _ROLES_MANAGE_ROLE_PERMISSION))

validateRoleWorkspacePermissions :: WizardRequestContextC s m => [String] -> Maybe U.UUID -> RoleChangeDTO -> m ()
validateRoleWorkspacePermissions organizationOnly mWorkspaceUuid dto =
  when (isJust mWorkspaceUuid) $
    forM_ dto.permissions $ \permission ->
      when (permission `elem` organizationOnly) (throwError . UserError $ _ERROR_VALIDATION__USER_ROLE_ORGANIZATION_ONLY_PERMISSION permission)
