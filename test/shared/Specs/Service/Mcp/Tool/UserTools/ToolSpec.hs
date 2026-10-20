module Specs.Service.Mcp.Tool.UserTools.ToolSpec (
  userToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.UserTools.DeleteUser
import Specs.Service.Mcp.Tool.UserTools.InviteUsers
import Specs.Service.Mcp.Tool.UserTools.ListRoles
import Specs.Service.Mcp.Tool.UserTools.SearchUsers
import Specs.Service.Mcp.Tool.UserTools.UpdateUser

userToolsSpec :: RequestContext -> SpecWith ((), Application)
userToolsSpec requestContext = do
  search_users requestContext
  update_user requestContext
  invite_users requestContext
  delete_user requestContext
  list_roles requestContext
