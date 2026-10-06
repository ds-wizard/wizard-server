module Shared.Api.Resource.User.Group.UserGroupSuggestionJM where

import Data.Aeson

import Shared.Model.User.UserGroupSuggestion
import Shared.Util.Aeson

instance FromJSON UserGroupSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupSuggestion where
  toJSON = genericToJSON jsonOptions
