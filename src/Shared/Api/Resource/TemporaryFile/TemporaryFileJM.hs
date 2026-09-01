module Shared.Api.Resource.TemporaryFile.TemporaryFileJM where

import Data.Aeson

import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Util.Aeson

instance FromJSON TemporaryFileDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemporaryFileDTO where
  toJSON = genericToJSON jsonOptions
