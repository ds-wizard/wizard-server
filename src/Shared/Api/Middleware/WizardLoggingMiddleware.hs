module Shared.Api.Middleware.WizardLoggingMiddleware where

import qualified Data.Text as T
import Network.Wai (Middleware)

import Shared.Api.Middleware.LoggingMiddleware
import Shared.Service.UserToken.UserTokenUtil
import Shared.Util.Token

loggingMiddleware :: String -> Middleware
loggingMiddleware = createLoggingMiddleware extractIdentity

extractIdentity :: T.Text -> Maybe String
extractIdentity tokenHeader = separateToken (T.unpack tokenHeader) >>= getUserUuidFromToken
