module Shared.Api.Resource.TemporaryFile.TemporaryFileJM where

import Data.Aeson

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON TemporaryFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemporaryFileDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TemporaryFileDTO where
  jsonSchema = genericJsonSchema jsonOptions
