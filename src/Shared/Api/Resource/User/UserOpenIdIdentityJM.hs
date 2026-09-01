module Shared.Api.Resource.User.UserOpenIdIdentityJM where

import Data.Aeson

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Util.Aeson

instance FromJSON UserOpenIdIdentityDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserOpenIdIdentityDTO where
  toJSON = genericToJSON jsonOptions
