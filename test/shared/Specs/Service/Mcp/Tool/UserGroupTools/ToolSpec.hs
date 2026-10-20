module Specs.Service.Mcp.Tool.UserGroupTools.ToolSpec (
  userGroupToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.UserGroupTools.DeleteUserGroup
import Specs.Service.Mcp.Tool.UserGroupTools.ListUserGroups
import Specs.Service.Mcp.Tool.UserGroupTools.SaveUserGroup

userGroupToolsSpec :: RequestContext -> SpecWith ((), Application)
userGroupToolsSpec requestContext = do
  list_user_groups requestContext
  save_user_group requestContext
  delete_user_group requestContext
