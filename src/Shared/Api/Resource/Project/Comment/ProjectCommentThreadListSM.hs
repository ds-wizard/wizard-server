module Shared.Api.Resource.Project.Comment.ProjectCommentThreadListSM where

import Data.Swagger

import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Model.Project.Comment.ProjectCommentList
import Shared.Util.Swagger

instance ToSchema ProjectCommentThreadList where
  declareNamedSchema = toSwagger cmtQ1_t1Dto

instance ToSchema ProjectCommentList where
  declareNamedSchema = toSwagger cmtQ1_t1_1Dto
