module Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO
import Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlJM ()
import Shared.Util.Swagger

instance ToSchema OpenIdClientAuthenticationUrlDTO where
  declareNamedSchema =
    toSwagger
      OpenIdClientAuthenticationUrlDTO
        { url = "https://idp.example.com/authorize?client_id=my-client&state=someState&nonce=someNonce"
        , state = "someState"
        }
