module Shared.Api.Resource.Project.Detail.ProjectDetailReportSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Report.ReportSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Report.Data.Reports
import Shared.Model.Project.Project
import Shared.Model.Report.Report
import Shared.Util.Swagger

instance ToSchema ProjectDetailReportDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailReportDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackage = germanyPackageSuggestion
        , locale = Nothing
        , isTemplate = project1.isTemplate
        , permissions = [project1AlbertEditProjectPermDto]
        , fileCount = 0
        , totalReport = report1.totalReport
        , chapterReports = report1.chapterReports
        , chapters = report1.chapters
        , metrics = report1.metrics
        }
