module Shared.Api.Resource.Dev.DevOperationJM where

import Data.Aeson

import Shared.Api.Resource.Dev.DevJM ()
import Shared.Api.Resource.Dev.DevOperationDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DevOperationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperationDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DevOperationDTO where
  jsonSchema = genericJsonSchema jsonOptions
