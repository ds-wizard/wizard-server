module Specs.Service.Mcp.Tool.UserAccountTools.ToolSpec (
  userAccountToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.UserAccountTools.SendUserEmailLink
import Specs.Service.Mcp.Tool.UserAccountTools.SetUserPassword

userAccountToolsSpec :: RequestContext -> SpecWith ((), Application)
userAccountToolsSpec requestContext = do
  set_user_password requestContext
  send_user_email_link requestContext
