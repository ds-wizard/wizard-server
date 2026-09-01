module Shared.Api.Resource.Document.DocumentCreateJM where

import Data.Aeson

import Shared.Api.Resource.Document.DocumentCreateDTO
import Shared.Util.Aeson

instance FromJSON DocumentCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentCreateDTO where
  toJSON = genericToJSON jsonOptions
