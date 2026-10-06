module Shared.Api.Resource.Plugin.PluginSettingsJM where

import Data.Aeson

import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Util.Aeson

instance FromJSON PluginSettingsListDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PluginSettingsListDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PluginSettingsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PluginSettingsChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON WorkspacePluginSettingsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WorkspacePluginSettingsDTO where
  toJSON = genericToJSON jsonOptions
