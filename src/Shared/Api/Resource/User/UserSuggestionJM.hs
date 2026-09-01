module Shared.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Model.User.UserSuggestion
import Shared.Util.Aeson

instance FromJSON UserSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSuggestion where
  toJSON = genericToJSON jsonOptions
