module WizardServer.Api.Resource.Config.ClientConfigSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleSM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Shared.Api.Resource.Plugin.PluginListSM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import qualified Shared.Database.Migration.Development.Tenant.Data.TenantConfigs as STC
import qualified Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs as TC
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Model.Config.ServerConfigDM as S_S
import Shared.Model.Config.WizardServerConfig ()
import qualified Shared.Model.Config.WizardServerConfigDM as S
import Shared.Service.User.WizardUserMapper
import Shared.Util.Swagger
import WizardServer.Api.Resource.Config.ClientConfigDTO
import WizardServer.Api.Resource.Config.ClientConfigJM ()
import WizardServer.Api.Resource.Tenant.Config.TenantConfigSM ()
import WizardServer.Api.Resource.User.UserProfileSM ()
import WizardServer.Service.Config.Client.ClientConfigMapper

instance ToSchema ClientConfigDTO where
  declareNamedSchema = toSwaggerWithType "type" (toClientConfigDTO S.defaultConfig TC.defaultOrganization TC.defaultAuthentication [defaultOpenIdClientSimple] TC.defaultPrivacyAndSupport TC.defaultDashboardAndLoginScreen STC.defaultLookAndFeel TC.defaultRegistry TC.defaultProject TC.defaultSubmission STC.defaultFeatures TC.defaultOwl (Just $ toUserProfile (toDTO userAlbert) [] M.empty) [] [plugin1List] M.empty defaultTenantModules defaultTenant)

instance ToSchema ClientConfigAuthDTO where
  declareNamedSchema = toSwagger (toClientAuthDTO TC.defaultAuthentication [defaultOpenIdClientSimple])

instance ToSchema ClientConfigAuthExternalDTO where
  declareNamedSchema = toSwagger (toClientAuthExternalDTO [defaultOpenIdClientSimple])

instance ToSchema ClientConfigRegistryDTO where
  declareNamedSchema = toSwagger (toClientConfigRegistryDTO S.defaultRegistry TC.defaultRegistry)

instance ToSchema ClientConfigProjectDTO where
  declareNamedSchema = toSwagger (toClientConfigProjectDTO TC.defaultProject)

instance ToSchema ClientConfigCloudDTO where
  declareNamedSchema = toSwagger (toClientConfigCloudDTO S_S.defaultCloud defaultTenant)

instance ToSchema ClientConfigAdminDTO where
  declareNamedSchema = toSwagger (toClientConfigAdminDTO S.defaultAdmin)

instance ToSchema ClientConfigFeaturesDTO where
  declareNamedSchema = toSwagger (toClientConfigFeaturesDTO S.defaultAdmin STC.defaultFeatures)

instance ToSchema ClientConfigSignalBridgeDTO where
  declareNamedSchema = toSwagger (toClientConfigSignalBridgeDTO S_S.defaultCloud)

instance ToSchema ClientConfigModuleDTO where
  declareNamedSchema = toSwagger (toModuleDTO defaultTenantModule)
