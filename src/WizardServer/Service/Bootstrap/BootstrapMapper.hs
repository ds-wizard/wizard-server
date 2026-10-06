module WizardServer.Service.Bootstrap.BootstrapMapper where

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Model.Config.WizardServerConfig
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Model.Settings.Settings
import Shared.Model.Tenant.Module.TenantModule
import Shared.Model.Tenant.Tenant
import Shared.Model.User.UserProfile
import Shared.Service.Acl.AclService (workspaceRolesPermissions)
import Shared.Service.Bootstrap.BootstrapMapper
import WizardServer.Api.Resource.Bootstrap.BootstrapDTO
import WizardServer.Model.Bootstrap.BootstrapSettings

toBootstrapDTO :: ServerConfig -> Tenant -> BootstrapSettings -> [OpenIdClientSimple] -> Maybe UserProfile -> [String] -> [TenantModule] -> BootstrapPlugins -> BootstrapDTO
toBootstrapDTO serverConfig tenant settings openIdClients mUserProfile tours tenantModules bootstrapPlugins =
  BootstrapDTO
    { user = mUserProfile
    , tours = tours
    , authentication = BootstrapAuthenticationDTO {internal = settings.authentication, openId = openIdClients}
    , loginScreen = settings.loginScreen
    , features = settings.features {aiAssistantEnabled = serverConfig.admin.enabled && settings.features.aiAssistantEnabled}
    , privacy = toPrivacyDTO serverConfig.general.privacyUrl serverConfig.general.termsOfServiceUrl
    , lookAndFeel = settings.lookAndFeel
    , users = settings.users
    , registry = BootstrapRegistryDTO {enabled = settings.registry.enabled, url = serverConfig.registry.clientUrl}
    , cloud = toCloudDTO serverConfig.cloud tenant
    , admin = BootstrapAdminDTO {enabled = serverConfig.admin.enabled}
    , signalBridge = toSignalBridgeDTO serverConfig.cloud
    , modules = if serverConfig.admin.enabled then maybe [] (\profile -> toModuleDTOs (profilePermissions tenant profile) tenantModules) mUserProfile else []
    , plugins = bootstrapPlugins.plugins
    , pluginSettings = bootstrapPlugins.pluginSettings
    , multiWorkspace = tenant.multiWorkspace
    }

profilePermissions :: Tenant -> UserProfile -> [String]
profilePermissions tenant profile = profile.organizationRole.permissions ++ workspaceRolesPermissions profile.workspaceRoles tenant.multiWorkspace
