module Shared.Api.Resource.UserToken.LoginSM where

import Data.Swagger

import Shared.Api.Resource.UserToken.LoginDTO
import Shared.Api.Resource.UserToken.LoginJM ()
import Shared.Util.Swagger

instance ToSchema LoginDTO where
  declareNamedSchema = toSwagger (LoginDTO {email = "albert.einstein@example.com", password = "password", code = Nothing})
