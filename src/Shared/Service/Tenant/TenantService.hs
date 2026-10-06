module Shared.Service.Tenant.TenantService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.TenantDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleDM
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Config.TenantConfigMail
import Shared.Model.Tenant.Config.TenantConfigMailDM
import Shared.Model.Tenant.Tenant
import Shared.Model.Tenant.TenantSuggestion
import Shared.Model.User.Role
import Shared.Service.Settings.SettingsCreationService
import Shared.Service.Settings.SettingsService
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantMapper
import Shared.Service.Tenant.TenantUtil
import Shared.Service.Tenant.TenantValidation
import Shared.Service.Tenant.Usage.WizardUsageService
import Shared.Service.User.UserService
import qualified Shared.Service.User.WizardUserMapper as U_Mapper
import Shared.Service.Workspace.WorkspaceService
import Shared.Util.Crypto
import Shared.Util.Uuid

getTenantsPage :: WizardRequestContextC s m => Maybe String -> Maybe [TenantState] -> Maybe Bool -> Pageable -> [Sort] -> m (Page TenantDTO)
getTenantsPage mQuery mStates mEnabled pageable sort = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenants <- findTenantsPage mQuery mStates mEnabled pageable sort
  traverse enhanceTenant tenants

getTenantSuggestions :: WizardRequestContextC s m => Maybe String -> m [TenantSuggestion]
getTenantSuggestions mQuery = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  findTenantSuggestions mQuery

registerOrCreateTenantByAdmin :: WizardRequestContextC s m => TenantCreateDTO -> m TenantDTO
registerOrCreateTenantByAdmin reqDto = do
  hasPermission <- hasPermission _TENANTS_MANAGE_ROLE_PERMISSION
  if hasPermission
    then createTenantByAdmin reqDto
    else registerTenant reqDto

registerTenant :: WizardRequestContextC s m => TenantCreateDTO -> m TenantDTO
registerTenant reqDto = do
  runInTransaction $ do
    validateTenantCreateDTO reqDto False
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks (.serverConfig')
    let tenant = fromRegisterCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    createDefaultWorkspace uuid tenant.name now
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO reqDto adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl True
    createConfig uuid adminRole.uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByAdmin :: WizardRequestContextC s m => TenantCreateDTO -> m TenantDTO
createTenantByAdmin reqDto = do
  runInTransaction $ do
    checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
    validateTenantCreateDTO reqDto True
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks (.serverConfig')
    let tenant = fromAdminCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    createDefaultWorkspace uuid tenant.name now
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO (reqDto {password = userPassword}) adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl False
    createConfig uuid adminRole.uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

getTenantByUuid :: WizardRequestContextC s m => U.UUID -> m TenantDetailDTO
getTenantByUuid uuid = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenant <- findTenantByUuid uuid
  usage <- getUsage uuid
  allUsers <- findUsersWithTenantFiltered uuid []
  let users = filter (elem _USERS_MANAGE_ROLE_PERMISSION . (.role.permissions)) allUsers
  lookAndFeel <- getSettingsByTenantUuid findSettingsLookAndFeel uuid
  return $ toDetailDTO tenant lookAndFeel.logoUrl lookAndFeel.primaryColor usage users

modifyTenant :: WizardRequestContextC s m => U.UUID -> TenantChangeDTO -> m Tenant
modifyTenant uuid reqDto = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenant <- findTenantByUuid uuid
  validateTenantChangeDTO tenant reqDto
  serverConfig <- asks (.serverConfig')
  let updatedTenant = fromChangeDTO tenant reqDto serverConfig
  updateTenantByUuid updatedTenant

deleteTenant :: WizardRequestContextC s m => U.UUID -> m ()
deleteTenant uuid = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  _ <- findTenantByUuid uuid
  deleteTenantByUuid uuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAdminRole :: WizardRequestContextC s m => U.UUID -> UTCTime -> m Role
createAdminRole tenantUuid now = do
  uuid <- liftIO generateUuid
  let role =
        Role
          { uuid = uuid
          , name = "Admin"
          , permissions = allRolePermissions
          , isAdmin = True
          , tenantUuid = tenantUuid
          , createdAt = now
          , updatedAt = now
          , workspaceUuid = Nothing
          }
  insertRole role
  return role

createConfig :: WizardRequestContextC s m => U.UUID -> U.UUID -> UTCTime -> m ()
createConfig uuid defaultRoleUuid now =
  runInTransaction $ do
    createSettings uuid defaultRoleUuid
    void $ insertTenantConfigMail (defaultMail {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigMail)

createLocale :: WizardRequestContextC s m => U.UUID -> UTCTime -> m Locale
createLocale tntUuid now = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    let locale =
          localeDefault
            { uuid = uuid
            , tenantUuid = tntUuid
            , createdAt = now
            , updatedAt = now
            }
            :: Locale
    insertLocale locale
    return locale
