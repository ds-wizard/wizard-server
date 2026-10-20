module Specs.Service.Mcp.Tool.WorkspaceTools.ToolSpec (
  workspaceToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.WorkspaceTools.DeleteWorkspace
import Specs.Service.Mcp.Tool.WorkspaceTools.ListWorkspaceMembers
import Specs.Service.Mcp.Tool.WorkspaceTools.ListWorkspaces
import Specs.Service.Mcp.Tool.WorkspaceTools.SaveWorkspace
import Specs.Service.Mcp.Tool.WorkspaceTools.SetWorkspaceMembers

workspaceToolsSpec :: RequestContext -> SpecWith ((), Application)
workspaceToolsSpec requestContext = do
  list_workspaces requestContext
  list_workspace_members requestContext
  save_workspace requestContext
  set_workspace_members requestContext
  delete_workspace requestContext
