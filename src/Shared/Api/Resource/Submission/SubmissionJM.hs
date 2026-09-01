module Shared.Api.Resource.Submission.SubmissionJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList
import Shared.Util.Aeson

instance FromJSON SubmissionState

instance ToJSON SubmissionState

instance FromJSON SubmissionList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionList where
  toJSON = genericToJSON jsonOptions
