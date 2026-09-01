module WizardServer.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Api.Resource.Plugin.PluginListJM ()
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Util.Aeson
import WizardServer.Api.Resource.Config.ClientConfigDTO
import WizardServer.Api.Resource.User.UserProfileJM ()

instance FromJSON ClientConfigDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientConfigDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ClientConfigAuthDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAuthExternalDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthExternalDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigRegistryDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigProjectDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigProjectDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigCloudDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigCloudDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAdminDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAdminDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigFeaturesDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigFeaturesDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigSignalBridgeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigSignalBridgeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigModuleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigModuleDTO where
  toJSON = genericToJSON jsonOptions
