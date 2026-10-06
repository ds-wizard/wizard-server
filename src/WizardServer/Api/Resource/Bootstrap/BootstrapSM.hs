module WizardServer.Api.Resource.Bootstrap.BootstrapSM where

import Data.Swagger

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO

import Shared.Api.Resource.Bootstrap.BootstrapSM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleSM ()
import Shared.Api.Resource.Plugin.PluginListSM ()
import Shared.Api.Resource.Settings.SettingsSM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Database.Migration.Development.Plugin.Data.PluginSettings
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Config.WizardServerConfig
import qualified Shared.Model.Config.WizardServerConfigDM as S
import Shared.Model.Settings.SettingsDM
import Shared.Util.Swagger
import WizardServer.Api.Resource.Bootstrap.BootstrapDTO
import WizardServer.Api.Resource.Bootstrap.BootstrapJM ()
import WizardServer.Api.Resource.User.UserProfileSM ()
import WizardServer.Model.Bootstrap.BootstrapSettings
import WizardServer.Service.Bootstrap.BootstrapMapper

bootstrapSettingsExample :: BootstrapSettings
bootstrapSettingsExample =
  BootstrapSettings
    { authentication = defaultSettingsAuthentication
    , loginScreen = defaultSettingsLoginScreen
    , features = defaultSettingsFeatures
    , lookAndFeel = defaultSettingsLookAndFeel
    , users = defaultSettingsUsers
    , registry = defaultSettingsRegistry
    }

instance ToSchema BootstrapDTO where
  declareNamedSchema = toSwaggerWithType "type" (toBootstrapDTO S.defaultConfig defaultTenant bootstrapSettingsExample [defaultOpenIdClientSimple] (Just userAlbertProfile) [] defaultTenantModules (BootstrapPlugins [plugin1List] plugin1Dict))

instance ToSchema BootstrapAuthenticationDTO where
  declareNamedSchema = toSwagger (BootstrapAuthenticationDTO defaultSettingsAuthentication [defaultOpenIdClientSimple])

instance ToSchema BootstrapRegistryDTO where
  declareNamedSchema = toSwagger (BootstrapRegistryDTO True S.defaultRegistry.clientUrl)

instance ToSchema BootstrapAdminDTO where
  declareNamedSchema = toSwagger (BootstrapAdminDTO False)
