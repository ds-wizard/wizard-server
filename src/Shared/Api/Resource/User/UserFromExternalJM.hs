module Shared.Api.Resource.User.UserFromExternalJM where

import Data.Aeson

import Shared.Api.Resource.User.UserFromExternalDTO
import Shared.Util.Aeson

instance FromJSON UserFromExternalDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserFromExternalDTO where
  toJSON = genericToJSON jsonOptions
