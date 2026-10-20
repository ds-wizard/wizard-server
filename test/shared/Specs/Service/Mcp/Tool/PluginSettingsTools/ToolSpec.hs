module Specs.Service.Mcp.Tool.PluginSettingsTools.ToolSpec (
  pluginSettingsToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.PluginSettingsTools.GetPluginSettings
import Specs.Service.Mcp.Tool.PluginSettingsTools.ListPluginSettings
import Specs.Service.Mcp.Tool.PluginSettingsTools.UpdatePluginSettings

pluginSettingsToolsSpec :: RequestContext -> SpecWith ((), Application)
pluginSettingsToolsSpec requestContext = do
  list_plugin_settings requestContext
  get_plugin_settings requestContext
  update_plugin_settings requestContext
