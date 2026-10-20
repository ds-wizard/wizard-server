module Shared.Api.Resource.Dev.DevJM where

import Data.Aeson

import Shared.Model.Dev.Dev
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DevOperationParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperationParameter where
  toJSON = genericToJSON jsonOptions

instance FromJSON DevOperationParameterType

instance ToJSON DevOperationParameterType

instance JsonSchema DevOperationParameter where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema DevOperationParameterType where
  jsonSchema = genericJsonSchema defaultOptions
