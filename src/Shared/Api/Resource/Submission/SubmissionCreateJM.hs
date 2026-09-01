module Shared.Api.Resource.Submission.SubmissionCreateJM where

import Data.Aeson

import Shared.Api.Resource.Submission.SubmissionCreateDTO
import Shared.Util.Aeson

instance FromJSON SubmissionCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SubmissionCreateDTO where
  toJSON = genericToJSON jsonOptions
