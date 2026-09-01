module Shared.Api.Resource.Project.ProjectReportDTO where

import GHC.Generics

import Shared.Model.Report.Report

data ProjectReportDTO = ProjectReportDTO
  { indications :: [Indication]
  }
  deriving (Show, Eq, Generic)
