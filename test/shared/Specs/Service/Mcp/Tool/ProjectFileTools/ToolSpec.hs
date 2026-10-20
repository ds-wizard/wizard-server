module Specs.Service.Mcp.Tool.ProjectFileTools.ToolSpec (
  projectFileToolsSpec,
) where

import Network.Wai (Application)
import Test.Hspec

import WizardServer.Model.Context.RequestContext

import Specs.Service.Mcp.Tool.ProjectFileTools.DeleteProjectFile
import Specs.Service.Mcp.Tool.ProjectFileTools.DownloadProjectFile
import Specs.Service.Mcp.Tool.ProjectFileTools.ListProjectFiles
import Specs.Service.Mcp.Tool.ProjectFileTools.UploadProjectFile

projectFileToolsSpec :: RequestContext -> SpecWith ((), Application)
projectFileToolsSpec requestContext = do
  list_project_files requestContext
  upload_project_file requestContext
  download_project_file requestContext
  delete_project_file requestContext
