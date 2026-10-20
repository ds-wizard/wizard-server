module Shared.Api.Resource.Version.VersionJM where

import Data.Aeson

import Shared.Api.Resource.Version.VersionDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON VersionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON VersionDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema VersionDTO where
  jsonSchema = genericJsonSchema jsonOptions
