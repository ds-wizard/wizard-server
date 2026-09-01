module Shared.Api.Resource.Auth.AuthConsentSM where

import Data.Swagger

import Shared.Api.Resource.Auth.AuthConsentDTO
import Shared.Api.Resource.Auth.AuthConsentJM ()
import Shared.Util.Swagger

instance ToSchema AuthConsentDTO where
  declareNamedSchema = toSwagger (AuthConsentDTO {hash = "123", sessionState = Just "456"})
