module Shared.Api.Resource.Common.MapEntryJM where

import Data.Aeson

import Shared.Model.Common.MapEntry
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance (ToJSON key, ToJSON value) => ToJSON (MapEntry key value) where
  toJSON = genericToJSON jsonOptions

instance (FromJSON key, FromJSON value) => FromJSON (MapEntry key value) where
  parseJSON = genericParseJSON jsonOptions

instance (JsonSchema key, JsonSchema value) => JsonSchema (MapEntry key value) where
  jsonSchema = genericJsonSchema jsonOptions
