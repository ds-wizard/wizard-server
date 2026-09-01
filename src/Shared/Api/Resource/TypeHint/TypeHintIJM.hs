module Shared.Api.Resource.TypeHint.TypeHintIJM where

import Data.Aeson

import Shared.Integration.Resource.TypeHint.TypeHintIDTO
import Shared.Util.Aeson

instance FromJSON TypeHintIDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintIDTO where
  toJSON = genericToJSON jsonOptions
