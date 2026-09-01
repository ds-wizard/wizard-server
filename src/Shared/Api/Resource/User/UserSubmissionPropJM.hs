module Shared.Api.Resource.User.UserSubmissionPropJM where

import Data.Aeson

import Shared.Model.User.UserSubmissionProp
import Shared.Util.Aeson

instance FromJSON UserSubmissionProp where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionProp where
  toJSON = genericToJSON jsonOptions
