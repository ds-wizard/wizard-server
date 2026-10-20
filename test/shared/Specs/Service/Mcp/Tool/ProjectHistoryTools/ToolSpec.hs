module Specs.Service.Mcp.Tool.ProjectHistoryTools.ToolSpec (
  projectHistoryToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ProjectHistoryTools.DeleteProjectVersion
import Specs.Service.Mcp.Tool.ProjectHistoryTools.GetProjectEvent
import Specs.Service.Mcp.Tool.ProjectHistoryTools.ListProjectEvents
import Specs.Service.Mcp.Tool.ProjectHistoryTools.ListProjectVersions
import Specs.Service.Mcp.Tool.ProjectHistoryTools.RevertProject
import Specs.Service.Mcp.Tool.ProjectHistoryTools.SaveProjectVersion

projectHistoryToolsSpec :: RequestContext -> SpecWith ((), Application)
projectHistoryToolsSpec requestContext = do
  list_project_versions requestContext
  save_project_version requestContext
  delete_project_version requestContext
  list_project_events requestContext
  get_project_event requestContext
  revert_project requestContext
