module Shared.Api.Resource.Project.ProjectContentDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.ProjectReply
import Shared.Model.Project.Version.ProjectVersionList

data ProjectContentDTO = ProjectContentDTO
  { phaseUuid :: Maybe U.UUID
  , replies :: M.Map String Reply
  , commentThreadsMap :: M.Map String [ProjectCommentThreadList]
  , labels :: M.Map String [U.UUID]
  , events :: [ProjectEventList]
  , versions :: [ProjectVersionList]
  }
  deriving (Show, Eq, Generic)
