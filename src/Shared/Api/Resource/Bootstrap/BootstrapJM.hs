module Shared.Api.Resource.Bootstrap.BootstrapJM where

import Data.Aeson

import Shared.Api.Resource.Bootstrap.BootstrapCommonDTO
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Util.Aeson

instance FromJSON BootstrapPrivacyDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapPrivacyDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapCloudDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapCloudDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapSignalBridgeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapSignalBridgeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON BootstrapModuleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BootstrapModuleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON WorkspaceBootstrapDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WorkspaceBootstrapDTO where
  toJSON = genericToJSON jsonOptions
