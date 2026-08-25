module Shared.Common.Api.Handler.Common.Error where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..), encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import GHC.Records
import Network.HTTP.Types.Status
import Servant (ServerError (..), err302, err400, err401, err403, err404, err500, errBody, errHeaders)
import Prelude hiding (log)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Constant.Api (contentTypeHeaderJSON)
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Model.Sentry.SentryEvent
import Shared.Common.Service.Sentry.SentryService
import Shared.Common.Util.Logger
import Shared.Common.Util.Sentry

sendError
  :: (MonadReader context m, HasField "serverConfig'" context sc, HasField "sentry'" sc ServerConfigSentry, HasField "buildInfoConfig'" context BuildInfoConfig, MonadLogger m, MonadIO m)
  => AppError
  -> m ServerError
sendError AcceptedError =
  return $
    ServerError
      { errHTTPCode = 202
      , errReasonPhrase = "Accepted"
      , errBody = encode AcceptedError
      , errHeaders = [contentTypeHeaderJSON]
      }
sendError (MovedPermanentlyError url) =
  return $
    ServerError
      { errHTTPCode = 301
      , errReasonPhrase = "Moved Permanently"
      , errBody = encode $ MovedPermanentlyError url
      , errHeaders =
          [ contentTypeHeaderJSON
          , ("Location", BS.pack url)
          ]
      }
sendError (FoundError url) =
  return $ err302 {errBody = encode $ FoundError url, errHeaders = [contentTypeHeaderJSON, ("Location", BS.pack url)]}
sendError (ValidationError formErrors fieldErrors) =
  return $ err400 {errBody = encode $ ValidationError formErrors fieldErrors, errHeaders = [contentTypeHeaderJSON]}
sendError (UserError message) =
  return $ err400 {errBody = encode $ UserError message, errHeaders = [contentTypeHeaderJSON]}
sendError (SystemLogError message) =
  return $ err400 {errBody = encode $ SystemLogError message, errHeaders = [contentTypeHeaderJSON]}
sendError (UnauthorizedError message) =
  return $ err401 {errBody = encode $ UnauthorizedError message, errHeaders = [contentTypeHeaderJSON]}
sendError (ForbiddenError message) =
  return $ err403 {errBody = encode $ ForbiddenError message, errHeaders = [contentTypeHeaderJSON]}
sendError (NotExistsError message) =
  return $ err404 {errBody = encode $ NotExistsError message, errHeaders = [contentTypeHeaderJSON]}
sendError LockedError =
  return $
    ServerError
      { errHTTPCode = 423
      , errReasonPhrase = "Locked"
      , errBody = encode LockedError
      , errHeaders = [contentTypeHeaderJSON]
      }
sendError (GeneralServerError message) = do
  logError _CMP_API message
  captureSentryEvent
    (toSentryEvent "sendErrorLogger" (normalizeMessage message))
      { fingerprint = ["GeneralServerError", normalizeMessage message]
      , extra = [("message", String . T.pack $ message)]
      }
  return $ err500 {errBody = encode $ GeneralServerError message, errHeaders = [contentTypeHeaderJSON]}
sendError (HttpClientError status message) = do
  logError _CMP_API message
  return $
    ServerError
      { errHTTPCode = statusCode status
      , errReasonPhrase = BS.unpack . statusMessage $ status
      , errBody = BSL.pack message
      , errHeaders = [contentTypeHeaderJSON]
      }
