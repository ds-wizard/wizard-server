module Shared.Service.Mcp.Tool.ProjectReportTools where

import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Mcp.Project.McpProjectReportDTO
import Shared.Api.Resource.Mcp.Project.McpProjectReportJM ()
import Shared.Model.Context.AclContext
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.ProjectTools (projectUuidArgument)
import Shared.Service.Report.ReportService
import Shared.Util.JsonSchema

projectReportTools :: WizardRequestContextC s m => [McpTool m]
projectReportTools = [getProjectReportTool]

getProjectReportTool :: WizardRequestContextC s m => McpTool m
getProjectReportTool =
  McpTool
    { name = "get_project_report"
    , title = "Get project report"
    , description =
        "Get the report of a project: how many questions are answered in total and per chapter (also counting only those of the current phase) "
          ++ "and the measures of the knowledge model metrics, from 0 to 1"
    , inputSchema = argumentsSchema projectUuidArgument
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy McpProjectReportDTO)
    , annotations = readOnlyAnnotations
    , continuable = True
    , permissions = [_MCP_READ_ROLE_PERMISSION]
    , toolset = Just ProjectsMcpToolset
    , transactionState = NoTransaction
    , run = withArguments projectUuidArgument $ fmap toJSON . getMcpReportByProjectUuid
    }
