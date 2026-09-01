module Shared.Model.Project.Cache.ProjectCache where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Model.Project.Version.ProjectVersionList

data ProjectCache = ProjectCache
  { projectUuid :: U.UUID
  , questionnaire :: ProjectDetailQuestionnaireDTO
  , report :: ProjectDetailReportDTO
  , versions :: [ProjectVersionList]
  , questionnaireSourceUpdatedAt :: UTCTime
  , versionsSourceUpdatedAt :: UTCTime
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
