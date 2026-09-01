module WizardServer.Service.User.Role.RoleService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Service.Common
import qualified Shared.Service.User.RoleMapper as Mapper
import Shared.Util.Uuid
import WizardServer.Service.User.Role.RoleValidation

getRolesPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page RoleList)
getRolesPage mQuery pageable sort = do
  checkPermissionsAny [_SETTINGS_MANAGE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
  findRolesPage mQuery pageable sort

getRole :: WizardRequestContextC s m => U.UUID -> m RoleList
getRole uuid = do
  checkPermissionsAny [_SETTINGS_MANAGE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]
  findRoleListByUuid uuid

createRole :: WizardRequestContextC s m => RoleChangeDTO -> m RoleList
createRole reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    validateRoleChangeDTO reqDto
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let role = Mapper.fromCreateDTO reqDto uuid tenantUuid now
    insertRole role
    findRoleListByUuid uuid

modifyRole :: WizardRequestContextC s m => U.UUID -> RoleChangeDTO -> m RoleList
modifyRole uuid reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    validateRoleChangeDTO reqDto
    role <- findRoleByUuid uuid
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED)
    now <- liftIO getCurrentTime
    let updated = Mapper.fromChangeDTO role reqDto now
    updateRoleByUuid updated
    void $ updateUsersRoleByRole uuid reqDto.permissions reqDto.name
    findRoleListByUuid uuid

deleteRole :: WizardRequestContextC s m => U.UUID -> m ()
deleteRole uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    role <- findRoleByUuid uuid
    when role.isAdmin (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_DELETED)
    tcAuthentication <- findTenantConfigAuthentication
    when (tcAuthentication.defaultRoleUuid == uuid) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IS_DEFAULT)
    count <- countUsersByRole uuid
    when (count > 0) (throwError $ UserError _ERROR_VALIDATION__USER_ROLE_IN_USE)
    void $ deleteRoleByUuid uuid

checkIfAdminIsDisabled :: WizardRequestContextC s m => m ()
checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Role Endpoints" (\s -> not s.admin.enabled)
