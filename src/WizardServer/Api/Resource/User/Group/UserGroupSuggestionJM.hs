module WizardServer.Api.Resource.User.Group.UserGroupSuggestionJM where

import Data.Aeson

import Shared.Util.Aeson
import WizardServer.Model.User.UserGroupSuggestion

instance FromJSON UserGroupSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupSuggestion where
  toJSON = genericToJSON jsonOptions
