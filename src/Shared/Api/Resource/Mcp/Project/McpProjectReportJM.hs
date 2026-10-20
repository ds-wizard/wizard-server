module Shared.Api.Resource.Mcp.Project.McpProjectReportJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Project.McpProjectReportDTO
import Shared.Api.Resource.Report.ReportJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON McpProjectReportDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpProjectReportDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance ToJSON McpChapterReportDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpChapterReportDTO where
  jsonSchema = genericJsonSchema jsonOptions

instance ToJSON McpReportMetricDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpReportMetricDTO where
  jsonSchema = genericJsonSchema jsonOptions
