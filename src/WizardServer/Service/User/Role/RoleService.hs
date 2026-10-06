module WizardServer.Service.User.Role.RoleService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Database.DAO.Settings.SettingsRolesDAO
import Shared.Database.DAO.User.RoleDAO
import qualified Shared.Database.DAO.User.RoleDAO as RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Model.Workspace.Workspace
import Shared.Service.Common
import Shared.Service.Settings.SettingsService
import Shared.Service.User.RoleAcl
import qualified Shared.Service.User.RoleMapper as Mapper
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Uuid
import WizardServer.Service.User.Role.RoleValidation

getRolesPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page RoleList)
getRolesPage mQuery pageable sort = do
  scope <- asks (.scope')
  reachable <- case scope of
    WorkspaceScope workspaceUuid -> isWorkspaceReachable workspaceUuid
    _ -> return True
  if reachable
    then do
      mWorkspaceUuid <- requireTenantOrWorkspaceScope
      checkRoleReadPermission mWorkspaceUuid
      findRolesPage mWorkspaceUuid mQuery pageable sort
    else return $ emptyPage RoleDAO.pageLabel pageable

getRole :: WizardRequestContextC s m => U.UUID -> m RoleList
getRole uuid = do
  checkPermissionsAny [_ROLES_MANAGE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION, _MEMBERS_MANAGE_ROLE_PERMISSION]
  role <- findRoleByUuid uuid
  checkRoleReadPermission role.workspaceUuid
  findRoleListByUuid uuid

createRole :: WizardRequestContextC s m => RoleChangeDTO -> m RoleList
createRole reqDto =
  runInTransaction $ do
    mWorkspaceUuid <- requireTenantOrWorkspaceScope
    checkRoleManagePermission mWorkspaceUuid
    checkIfAdminIsDisabled
    validateRoleChangeDTO mWorkspaceUuid reqDto
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let role = Mapper.fromCreateDTO reqDto uuid tenantUuid mWorkspaceUuid now
    insertRole role
    findRoleListByUuid uuid

modifyRole :: WizardRequestContextC s m => U.UUID -> RoleChangeDTO -> m RoleList
modifyRole uuid reqDto =
  runInTransaction $ do
    checkPermission _ROLES_MANAGE_ROLE_PERMISSION
    role <- findRoleByUuid uuid
    checkRoleManagePermission role.workspaceUuid
    checkIfAdminIsDisabled
    validateRoleChangeDTO role.workspaceUuid reqDto
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED)
    now <- liftIO getCurrentTime
    let updated = Mapper.fromChangeDTO role reqDto now
    updateRoleByUuid updated
    void $ updateUsersRoleByRole uuid reqDto.permissions reqDto.name
    findRoleListByUuid uuid

deleteRole :: WizardRequestContextC s m => U.UUID -> m ()
deleteRole uuid =
  runInTransaction $ do
    checkPermission _ROLES_MANAGE_ROLE_PERMISSION
    role <- findRoleByUuid uuid
    checkRoleManagePermission role.workspaceUuid
    checkIfAdminIsDisabled
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_DELETED)
    checkRoleIsNotDefault role
    checkRoleIsNotInUse role
    void $ deleteRoleByUuid uuid

checkRoleIsNotDefault :: WizardRequestContextC s m => Role -> m ()
checkRoleIsNotDefault role = do
  defaultRoleUuid <-
    case role.workspaceUuid of
      Just workspaceUuid -> (.defaultRoleUuid) <$> findWorkspaceByUuid workspaceUuid
      Nothing -> Just . (.defaultRoleUuid) <$> getCurrentSettings findSettingsRoles
  when (defaultRoleUuid == Just role.uuid) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IS_DEFAULT)

checkRoleIsNotInUse :: WizardRequestContextC s m => Role -> m ()
checkRoleIsNotInUse role = do
  count <- maybe (countUsersByRole role.uuid) (const (countWorkspaceMembershipsByRole role.uuid)) role.workspaceUuid
  when (count > 0) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IN_USE)

checkIfAdminIsDisabled :: WizardRequestContextC s m => m ()
checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Role Endpoints" (\s -> not s.admin.enabled)
