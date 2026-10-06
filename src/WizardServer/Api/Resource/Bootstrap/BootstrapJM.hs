module WizardServer.Api.Resource.Bootstrap.BootstrapJM where

import Data.Aeson

import Shared.Api.Resource.Bootstrap.BootstrapJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import Shared.Api.Resource.Plugin.PluginListJM ()
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Util.Aeson
import WizardServer.Api.Resource.Bootstrap.BootstrapDTO
import WizardServer.Api.Resource.User.UserProfileJM ()

instance FromJSON BootstrapDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON BootstrapDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON BootstrapAuthenticationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapAuthenticationDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapRegistryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapRegistryDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapAdminDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapAdminDTO where
  toJSON = genericToJSON jsonOptions
