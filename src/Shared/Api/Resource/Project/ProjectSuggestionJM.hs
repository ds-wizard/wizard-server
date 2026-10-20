module Shared.Api.Resource.Project.ProjectSuggestionJM where

import Data.Aeson

import Shared.Model.Project.ProjectSuggestion
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON ProjectSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSuggestion where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectSuggestion where
  jsonSchema = genericJsonSchema jsonOptions
