module Shared.Api.Resource.Project.ProjectReportJM where

import Data.Aeson

import Shared.Api.Resource.Project.ProjectReportDTO
import Shared.Api.Resource.Report.ReportJM ()
import Shared.Util.Aeson

instance FromJSON ProjectReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectReportDTO where
  toJSON = genericToJSON jsonOptions
