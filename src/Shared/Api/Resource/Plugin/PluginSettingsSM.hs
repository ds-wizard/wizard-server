module Shared.Api.Resource.Plugin.PluginSettingsSM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Database.Migration.Development.Plugin.Data.Plugins
import Shared.Model.Plugin.Plugin
import Shared.Util.Swagger

instance ToSchema PluginSettingsListDTO where
  declareNamedSchema = toSwagger (PluginSettingsListDTO plugin1.uuid True (Just True) Nothing Nothing)

instance ToSchema PluginSettingsChangeDTO where
  declareNamedSchema = toSwagger (PluginSettingsChangeDTO True (Just True))
