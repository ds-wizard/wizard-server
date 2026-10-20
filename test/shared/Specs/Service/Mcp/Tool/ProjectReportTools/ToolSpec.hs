module Specs.Service.Mcp.Tool.ProjectReportTools.ToolSpec (
  projectReportToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ProjectReportTools.GetProjectReport

projectReportToolsSpec :: RequestContext -> SpecWith ((), Application)
projectReportToolsSpec requestContext = do
  get_project_report requestContext
