module Shared.Api.Resource.UserToken.LoginJM where

import Data.Aeson

import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Util.Aeson

instance FromJSON LoginDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LoginDTO where
  toJSON = genericToJSON jsonOptions
