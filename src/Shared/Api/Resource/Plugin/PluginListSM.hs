module Shared.Api.Resource.Plugin.PluginListSM where

import Data.Swagger

import Shared.Api.Resource.Plugin.PluginListJM ()
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Plugin.PluginList
import Shared.Util.Swagger

instance ToSchema PluginList where
  declareNamedSchema = toSwagger plugin1List
