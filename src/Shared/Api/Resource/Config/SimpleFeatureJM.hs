module Shared.Api.Resource.Config.SimpleFeatureJM where

import Data.Aeson

import Shared.Model.Config.SimpleFeature
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON SimpleFeature where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SimpleFeature where
  toJSON = genericToJSON jsonOptions

instance JsonSchema SimpleFeature where
  jsonSchema = genericJsonSchema jsonOptions
