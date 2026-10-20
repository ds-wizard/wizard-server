module Shared.Service.Report.ReportMapper where

import Data.Aeson (Value)
import qualified Data.List as L

import Shared.Api.Resource.Mcp.Project.McpProjectReportDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Report.Report

toDTO :: ProjectDetailQuestionnaireDTO -> Report -> Maybe Value -> ProjectDetailReportDTO
toDTO project report locale =
  ProjectDetailReportDTO
    { uuid = project.uuid
    , name = project.name
    , visibility = project.visibility
    , sharing = project.sharing
    , knowledgeModelPackage = project.knowledgeModelPackage
    , locale = locale
    , isTemplate = project.isTemplate
    , permissions = project.permissions
    , fileCount = project.fileCount
    , totalReport = report.totalReport
    , chapters = report.chapters
    , chapterReports = report.chapterReports
    , metrics = report.metrics
    }

toMcpReportDTO :: ProjectDetailReportDTO -> McpProjectReportDTO
toMcpReportDTO report =
  McpProjectReportDTO
    { totalReport = report.totalReport
    , chapterReports = fmap (toMcpChapterReportDTO report.chapters) report.chapterReports
    , metrics = fmap (\metric -> McpReportMetricDTO metric.uuid metric.title metric.abbreviation) report.metrics
    }

toMcpChapterReportDTO :: [Chapter] -> ChapterReport -> McpChapterReportDTO
toMcpChapterReportDTO chapters chapterReport =
  McpChapterReportDTO
    { chapterUuid = chapterReport.chapterUuid
    , title = maybe "" (.title) (L.find ((== chapterReport.chapterUuid) . (.uuid)) chapters)
    , indications = chapterReport.indications
    , metrics = chapterReport.metrics
    }
