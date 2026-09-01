module Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Util.Aeson

instance FromJSON ProjectCommentThreadList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadList where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectCommentList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentList where
  toJSON = genericToJSON jsonOptions
