module Shared.Util.Jwk where

import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.Types as RSA_Types
import qualified Jose.Jwk as JWK

toRsaPublicKey :: RSA.PrivateKey -> RSA.PublicKey
toRsaPublicKey = RSA_Types.toPublicKey . RSA_Types.KeyPair

toJwkSet :: RSA.PrivateKey -> JWK.JwkSet
toJwkSet key =
  JWK.JwkSet
    { keys = [JWK.RsaPublicJwk (toRsaPublicKey key) Nothing Nothing Nothing]
    }
