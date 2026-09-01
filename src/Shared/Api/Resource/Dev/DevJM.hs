module Shared.Api.Resource.Dev.DevJM where

import Data.Aeson

import Shared.Model.Dev.Dev
import Shared.Util.Aeson

instance FromJSON DevOperationParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevOperationParameter where
  toJSON = genericToJSON jsonOptions

instance FromJSON DevOperationParameterType

instance ToJSON DevOperationParameterType
