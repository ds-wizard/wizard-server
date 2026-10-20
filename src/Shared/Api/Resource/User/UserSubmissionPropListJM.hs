module Shared.Api.Resource.User.UserSubmissionPropListJM where

import Data.Aeson

import Shared.Model.User.UserSubmissionPropList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserSubmissionPropList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSubmissionPropList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserSubmissionPropList where
  jsonSchema = genericJsonSchema jsonOptions
