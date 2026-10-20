module Shared.Api.Resource.Dev.DevExecutionResultJM where

import Data.Aeson

import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON AdminExecutionResultDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AdminExecutionResultDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema AdminExecutionResultDTO where
  jsonSchema = genericJsonSchema jsonOptions
