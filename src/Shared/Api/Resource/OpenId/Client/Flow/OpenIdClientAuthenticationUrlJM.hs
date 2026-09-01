module Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlJM where

import Data.Aeson

import Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO
import Shared.Util.Aeson

instance FromJSON OpenIdClientAuthenticationUrlDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientAuthenticationUrlDTO where
  toJSON = genericToJSON jsonOptions
