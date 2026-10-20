module Specs.Service.Mcp.Tool.ProjectTools.ToolSpec (
  projectToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ProjectTools.CloneProject
import Specs.Service.Mcp.Tool.ProjectTools.CreateProject
import Specs.Service.Mcp.Tool.ProjectTools.DeleteProject
import Specs.Service.Mcp.Tool.ProjectTools.GetProject
import Specs.Service.Mcp.Tool.ProjectTools.GetProjectQuestionnaire
import Specs.Service.Mcp.Tool.ProjectTools.ListProjects
import Specs.Service.Mcp.Tool.ProjectTools.SetProjectReplies
import Specs.Service.Mcp.Tool.ProjectTools.SetProjectSharing
import Specs.Service.Mcp.Tool.ProjectTools.UpdateProjectSettings

projectToolsSpec :: RequestContext -> SpecWith ((), Application)
projectToolsSpec requestContext = do
  list_projects requestContext
  get_project requestContext
  create_project requestContext
  clone_project requestContext
  get_project_questionnaire requestContext
  set_project_replies requestContext
  update_project_settings requestContext
  set_project_sharing requestContext
  delete_project requestContext
