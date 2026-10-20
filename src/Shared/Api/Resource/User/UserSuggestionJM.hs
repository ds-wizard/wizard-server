module Shared.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Model.User.UserSuggestion
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSuggestion where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserSuggestion where
  jsonSchema = genericJsonSchema jsonOptions
