module Shared.Api.Resource.Submission.SubmissionSM where

import Data.Swagger

import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Submission.Data.Submissions
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Util.Swagger

instance ToSchema SubmissionState

instance ToSchema SubmissionList where
  declareNamedSchema = toSwagger submission1List
