module Specs.Service.Mcp.Tool.PrefabTools.ToolSpec (
  prefabToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.PrefabTools.ListIntegrationPrefabs

prefabToolsSpec :: RequestContext -> SpecWith ((), Application)
prefabToolsSpec = list_integration_prefabs
