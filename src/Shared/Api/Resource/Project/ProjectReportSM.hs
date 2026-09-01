module Shared.Api.Resource.Project.ProjectReportSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectReportDTO
import Shared.Api.Resource.Project.ProjectReportJM ()
import Shared.Api.Resource.Report.ReportSM ()
import Shared.Database.Migration.Development.Report.Data.Reports
import Shared.Util.Swagger

instance ToSchema ProjectReportDTO where
  declareNamedSchema = toSwagger projectReport
