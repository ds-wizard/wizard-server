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
import Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
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
import Shared.Model.PersistentCommand.Tenant.CreateTenantCommand
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.TenantConfigDM
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfigDM
import Shared.Model.Tenant.Tenant
import Shared.Model.Tenant.TenantSuggestion
import Shared.Model.User.Role
import Shared.Service.Tenant.Limit.WizardLimitService
import Shared.Service.Tenant.TenantMapper
import Shared.Service.Tenant.TenantUtil
import Shared.Service.Tenant.TenantValidation
import Shared.Service.Tenant.Usage.WizardUsageService
import Shared.Service.User.UserService
import qualified Shared.Service.User.WizardUserMapper as U_Mapper
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
  fmap toSuggestionUrls <$> findTenantSuggestions mQuery

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
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO reqDto adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid (tenantClientUrl tenant) True
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
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO (reqDto {password = userPassword}) adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid (tenantClientUrl tenant) False
    createConfig uuid adminRole.uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByCommand :: WizardRequestContextC s m => CreateTenantCommand -> m ()
createTenantByCommand command = do
  now <- liftIO getCurrentTime
  tenant <- findTenantByUuid command.uuid
  createWizardConfig tenant.uuid now

getTenantByUuid :: WizardRequestContextC s m => U.UUID -> m TenantDetailDTO
getTenantByUuid uuid = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenant <- findTenantByUuid uuid
  usage <- getUsage uuid
  allUsers <- findUsersWithTenantFiltered uuid []
  let users = filter (elem _USERS_MANAGE_ROLE_PERMISSION . (.role.permissions)) allUsers
  tcLookAndFeel <- findTenantConfigLookAndFeelByUuid uuid
  let mLogoUrl = tcLookAndFeel.logoUrl
  let mPrimaryColor = tcLookAndFeel.primaryColor
  return $ toDetailDTO tenant mLogoUrl mPrimaryColor usage users

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
          }
  insertRole role
  return role

createConfig :: WizardRequestContextC s m => U.UUID -> U.UUID -> UTCTime -> m ()
createConfig uuid defaultRoleUuid now = do
  runInTransaction $ do
    insertTenantConfigOrganization (defaultOrganization {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigOrganization)
    insertTenantConfigAuthentication (defaultAuthentication {tenantUuid = uuid, defaultRoleUuid = defaultRoleUuid, createdAt = now, updatedAt = now} :: TenantConfigAuthentication)
    insertTenantConfigPrivacyAndSupport (defaultPrivacyAndSupport {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigPrivacyAndSupport)
    insertTenantConfigDashboardAndLoginScreen (defaultDashboardAndLoginScreen {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigDashboardAndLoginScreen)
    insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigLookAndFeel)
    insertTenantConfigMail (defaultMail {tenantUuid = uuid, createdAt = now, updatedAt = now})
    insertTenantConfigFeatures (defaultFeatures {tenantUuid = uuid, createdAt = now, updatedAt = now})
    createWizardConfig uuid now

createWizardConfig :: WizardRequestContextC s m => U.UUID -> UTCTime -> m ()
createWizardConfig uuid now = do
  runInTransaction $ do
    insertTenantConfigRegistry (defaultRegistry {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigRegistry)
    insertTenantConfigProject (defaultProject {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigProject)
    insertTenantConfigSubmission (defaultSubmission {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigSubmission)
    void $ insertTenantConfigOwl (defaultOwl {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigOwl)

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
