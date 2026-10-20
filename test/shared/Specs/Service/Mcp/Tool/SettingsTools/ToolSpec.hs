module Specs.Service.Mcp.Tool.SettingsTools.ToolSpec (
  settingsToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.SettingsTools.GetSettings
import Specs.Service.Mcp.Tool.SettingsTools.UpdateSettings

settingsToolsSpec :: RequestContext -> SpecWith ((), Application)
settingsToolsSpec requestContext = do
  get_settings requestContext
  update_settings requestContext
