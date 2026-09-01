module Shared.Api.Resource.Project.Detail.ProjectDetailReportJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Api.Resource.Project.ProjectSharingJM ()
import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Api.Resource.Report.ReportJM ()
import Shared.Util.Aeson

instance FromJSON ProjectDetailReportDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailReportDTO where
  toJSON = genericToJSON jsonOptions
