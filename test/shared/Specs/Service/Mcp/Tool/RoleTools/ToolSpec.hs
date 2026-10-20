module Specs.Service.Mcp.Tool.RoleTools.ToolSpec (
  roleToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.RoleTools.DeleteRole
import Specs.Service.Mcp.Tool.RoleTools.SaveRole

roleToolsSpec :: RequestContext -> SpecWith ((), Application)
roleToolsSpec requestContext = do
  save_role requestContext
  delete_role requestContext
