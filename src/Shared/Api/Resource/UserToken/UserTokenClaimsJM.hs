module Shared.Api.Resource.UserToken.UserTokenClaimsJM where

import Data.Aeson

import Shared.Api.Resource.UserToken.UserTokenClaimsDTO
import Shared.Util.Aeson

instance FromJSON UserTokenClaimsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenClaimsDTO where
  toJSON = genericToJSON jsonOptions
