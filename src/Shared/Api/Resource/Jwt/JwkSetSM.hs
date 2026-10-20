module Shared.Api.Resource.Jwt.JwkSetSM where

import Data.Swagger
import Jose.Jwk

instance ToSchema JwkSet where
  declareNamedSchema _ = do
    return . NamedSchema (Just "JwkSet") $ mempty
