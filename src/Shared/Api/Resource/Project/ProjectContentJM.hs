module Shared.Api.Resource.Project.ProjectContentJM where

import Data.Aeson

import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Shared.Api.Resource.Project.Event.ProjectEventListJM ()
import Shared.Api.Resource.Project.ProjectContentDTO
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Util.Aeson

instance FromJSON ProjectContentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectContentDTO where
  toJSON = genericToJSON jsonOptions
