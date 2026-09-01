module Shared.Integration.Http.Admin.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import qualified Jose.Jwk as JWK
import Network.HTTP.Client (Response)

import Shared.Integration.Http.Common.ResponseMapper
import Shared.Model.Error.Error

toRetrieveJwtPublicKeysResponse :: Response BSL.ByteString -> Either AppError JWK.JwkSet
toRetrieveJwtPublicKeysResponse = extractResponseBody
