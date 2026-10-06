module WizardServer.Service.Bootstrap.BootstrapService where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import Shared.Database.DAO.Settings.SettingsAuthenticationDAO
import Shared.Database.DAO.Settings.SettingsFeaturesDAO
import Shared.Database.DAO.Settings.SettingsLoginScreenDAO
import Shared.Database.DAO.Settings.SettingsLookAndFeelDAO
import Shared.Database.DAO.Settings.SettingsRegistryDAO
import Shared.Database.DAO.Settings.SettingsUsersDAO
import Shared.Database.DAO.Tenant.Module.TenantModuleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.UserTourDAO
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Tenant
import Shared.Model.User.UserProfile
import Shared.Service.Bootstrap.BootstrapService
import Shared.Service.Settings.SettingsService
import Shared.Service.Tenant.TenantHelper
import Shared.Service.User.WizardUserMapper
import WizardServer.Api.Resource.Bootstrap.BootstrapDTO
import WizardServer.Database.DAO.User.UserGroupMembershipDAO
import WizardServer.Database.DAO.User.UserPluginSettingsDAO
import WizardServer.Model.Bootstrap.BootstrapSettings
import WizardServer.Service.Bootstrap.BootstrapMapper

getBootstrap :: WizardRequestContextC s m => Maybe String -> Maybe String -> m BootstrapDTO
getBootstrap mServerUrl mClientUrl = do
  serverConfig <- asks (.serverConfig')
  tenant <-
    if serverConfig.cloud.enabled
      then maybe getCurrentTenant findTenantByClientUrl mClientUrl
      else getCurrentTenant
  mHousekeeping <- checkBootstrapTenantState mServerUrl tenant
  case mHousekeeping of
    Just message -> return $ HousekeepingInProgressBootstrapDTO {message = message}
    Nothing -> do
      settings <- getBootstrapSettings tenant.uuid
      openIdClients <- findOpenIdClientDefinitionsSimpleByTenantUuid tenant.uuid
      mUserProfile <- getBootstrapUserProfile tenant.uuid
      tours <- getBootstrapTours
      tenantModules <- findTenantModulesByTenantUuid tenant.uuid
      bootstrapPlugins <- getBootstrapPlugins tenant.uuid
      return $ toBootstrapDTO serverConfig tenant settings openIdClients mUserProfile tours tenantModules bootstrapPlugins

getBootstrapSettings :: WizardRequestContextC s m => U.UUID -> m BootstrapSettings
getBootstrapSettings tenantUuid = do
  authentication <- getSettingsByTenantUuid findSettingsAuthentication tenantUuid
  loginScreen <- getSettingsByTenantUuid findSettingsLoginScreen tenantUuid
  features <- getSettingsByTenantUuid findSettingsFeatures tenantUuid
  lookAndFeel <- getSettingsByTenantUuid findSettingsLookAndFeel tenantUuid
  users <- getSettingsByTenantUuid findSettingsUsers tenantUuid
  registry <- getSettingsByTenantUuid findSettingsRegistry tenantUuid
  return BootstrapSettings {..}

getBootstrapUserProfile :: WizardRequestContextC s m => U.UUID -> m (Maybe UserProfile)
getBootstrapUserProfile tenantUuid = do
  mCurrentUser <- asks (.currentUser')
  case mCurrentUser of
    Just currentUser -> do
      userGroupUuids <- findUserGroupUuidsForUserUuidAndTenantUuid currentUser.uuid tenantUuid
      userPluginSettings <- findUserPluginSettingValuesByUserUuidAndTenantUuid currentUser.uuid tenantUuid
      workspaceRoles <- asks (.workspaceRoles')
      return . Just $ toUserProfile currentUser userGroupUuids userPluginSettings workspaceRoles
    Nothing -> return Nothing

getBootstrapTours :: WizardRequestContextC s m => m [String]
getBootstrapTours = do
  mCurrentUser <- asks (.currentUser')
  maybe (return []) (findUserToursByUserUuid . (.uuid)) mCurrentUser
