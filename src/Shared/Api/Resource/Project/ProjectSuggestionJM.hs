module Shared.Api.Resource.Project.ProjectSuggestionJM where

import Data.Aeson

import Shared.Model.Project.ProjectSuggestion
import Shared.Util.Aeson

instance FromJSON ProjectSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSuggestion where
  toJSON = genericToJSON jsonOptions
