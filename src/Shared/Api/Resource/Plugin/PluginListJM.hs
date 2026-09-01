module Shared.Api.Resource.Plugin.PluginListJM where

import Data.Aeson

import Shared.Model.Plugin.PluginList
import Shared.Util.Aeson

instance FromJSON PluginList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PluginList where
  toJSON = genericToJSON jsonOptions
