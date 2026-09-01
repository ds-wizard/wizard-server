module Shared.Api.Resource.Project.Comment.ProjectCommentThreadAssignedSM where

import Data.Swagger

import Shared.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Util.Swagger

instance ToSchema ProjectCommentThreadAssigned where
  declareNamedSchema = toSwagger cmtAssigned
