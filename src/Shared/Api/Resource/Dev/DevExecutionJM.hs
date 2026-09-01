module Shared.Api.Resource.Dev.DevExecutionJM where

import Data.Aeson

import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Util.Aeson

instance FromJSON DevExecutionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevExecutionDTO where
  toJSON = genericToJSON jsonOptions
