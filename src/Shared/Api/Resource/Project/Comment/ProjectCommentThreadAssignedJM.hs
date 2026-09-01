module Shared.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Util.Aeson

instance FromJSON ProjectCommentThreadAssigned where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadAssigned where
  toJSON = genericToJSON jsonOptions
