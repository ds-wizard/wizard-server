module Shared.Api.Resource.Project.Comment.ProjectCommentThreadNotificationJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSimpleJM ()
import Shared.Model.Project.Comment.ProjectCommentThreadNotification
import Shared.Util.Aeson

instance FromJSON ProjectCommentThreadNotification where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCommentThreadNotification where
  toJSON = genericToJSON jsonOptions
