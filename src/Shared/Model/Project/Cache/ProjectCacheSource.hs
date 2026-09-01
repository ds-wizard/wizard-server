module Shared.Model.Project.Cache.ProjectCacheSource where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ProjectCacheSource = ProjectCacheSource
  { projectUuid :: U.UUID
  , questionnaireSourceUpdatedAt :: UTCTime
  , versionsSourceUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
