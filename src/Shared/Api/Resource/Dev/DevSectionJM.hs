module Shared.Api.Resource.Dev.DevSectionJM where

import Data.Aeson

import Shared.Api.Resource.Dev.DevJM ()
import Shared.Api.Resource.Dev.DevOperationJM ()
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Util.Aeson

instance FromJSON DevSectionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DevSectionDTO where
  toJSON = genericToJSON jsonOptions
