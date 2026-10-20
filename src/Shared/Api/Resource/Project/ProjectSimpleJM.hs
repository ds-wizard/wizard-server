module Shared.Api.Resource.Project.ProjectSimpleJM where

import Data.Aeson

import Shared.Model.Project.ProjectSimple
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON ProjectSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSimple where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectSimple where
  jsonSchema = genericJsonSchema jsonOptions
