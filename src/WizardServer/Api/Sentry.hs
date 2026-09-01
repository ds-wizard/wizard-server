module WizardServer.Api.Sentry where

import Data.Aeson (Value (..), toJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Token

getSentryIdentity :: Maybe String -> [(String, Value)]
getSentryIdentity mAuthorizationHeader =
  [
    ( "user"
    , toJSON $ HashMap.fromList [("id", String . T.pack . fromMaybe "" . getUserUuidFromToken . fromMaybe "" . separateToken . fromMaybe "" $ mAuthorizationHeader)]
    )
  ]
