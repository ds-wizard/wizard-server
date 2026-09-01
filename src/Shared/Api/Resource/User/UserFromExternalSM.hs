module Shared.Api.Resource.User.UserFromExternalSM where

import Data.Swagger

import Shared.Api.Resource.User.UserFromExternalDTO
import Shared.Api.Resource.User.UserFromExternalJM ()
import Shared.Util.Swagger

instance ToSchema UserFromExternalDTO where
  declareNamedSchema =
    toSwagger
      UserFromExternalDTO
        { hash = "00000000-0000-0000-0000-000000000000"
        , email = "albert.einstein@example.com"
        , firstName = "Albert"
        , lastName = "Einstein"
        }
