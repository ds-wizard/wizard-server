module Specs.Service.Mcp.Tool.ProjectCommentTools.ToolSpec (
  projectCommentToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ProjectCommentTools.AddProjectComment
import Specs.Service.Mcp.Tool.ProjectCommentTools.AssignProjectCommentThread
import Specs.Service.Mcp.Tool.ProjectCommentTools.CreateProjectCommentThread
import Specs.Service.Mcp.Tool.ProjectCommentTools.DeleteProjectComment
import Specs.Service.Mcp.Tool.ProjectCommentTools.DeleteProjectCommentThread
import Specs.Service.Mcp.Tool.ProjectCommentTools.EditProjectComment
import Specs.Service.Mcp.Tool.ProjectCommentTools.ListProjectCommentThreads
import Specs.Service.Mcp.Tool.ProjectCommentTools.ResolveProjectCommentThread

projectCommentToolsSpec :: RequestContext -> SpecWith ((), Application)
projectCommentToolsSpec requestContext = do
  list_project_comment_threads requestContext
  create_project_comment_thread requestContext
  add_project_comment requestContext
  edit_project_comment requestContext
  resolve_project_comment_thread requestContext
  assign_project_comment_thread requestContext
  delete_project_comment_thread requestContext
  delete_project_comment requestContext
