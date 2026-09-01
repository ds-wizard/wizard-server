module Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO where

import Data.Aeson (Value)
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.Project
import Shared.Model.Report.Report

data ProjectDetailReportDTO = ProjectDetailReportDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: ProjectSharing
  , visibility :: ProjectVisibility
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , locale :: Maybe Value
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  , totalReport :: TotalReport
  , chapterReports :: [ChapterReport]
  , chapters :: [Chapter]
  , metrics :: [Metric]
  }
  deriving (Show, Eq, Generic)
