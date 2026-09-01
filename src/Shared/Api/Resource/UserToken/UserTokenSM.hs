module Shared.Api.Resource.UserToken.UserTokenSM where

import Data.Swagger

import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Api.Resource.UserToken.UserTokenJM ()
import Shared.Util.Date
import Shared.Util.Swagger

instance ToSchema UserTokenDTO where
  declareNamedSchema = toSwaggerWithFlatType "type" (UserTokenDTO {token = "someToken", expiresAt = dt' 2018 1 25})
