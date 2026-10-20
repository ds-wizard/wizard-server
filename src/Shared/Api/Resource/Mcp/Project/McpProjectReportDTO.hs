module Shared.Api.Resource.Mcp.Project.McpProjectReportDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Report.Report

data McpProjectReportDTO = McpProjectReportDTO
  { totalReport :: TotalReport
  , chapterReports :: [McpChapterReportDTO]
  , metrics :: [McpReportMetricDTO]
  }
  deriving (Show, Eq, Generic)

data McpChapterReportDTO = McpChapterReportDTO
  { chapterUuid :: U.UUID
  , title :: String
  , indications :: [Indication]
  , metrics :: [MetricSummary]
  }
  deriving (Show, Eq, Generic)

data McpReportMetricDTO = McpReportMetricDTO
  { uuid :: U.UUID
  , title :: String
  , abbreviation :: Maybe String
  }
  deriving (Show, Eq, Generic)
