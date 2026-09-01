module Shared.Common.Util.Logger (
  logDebugI,
  logInfoI,
  logWarnI,
  logErrorI,
  logDebug,
  logInfo,
  logWarn,
  logError,
  createLogRecord,
  showLogLevel,
  runLogging,
  f',
  LogLevel (..),
  module Shared.Common.Constant.Component,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogLevel (..), LogSource (..), LoggingT (..), MonadLogger, filterLogger, logWithoutLoc, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (Value (..))
import Data.IORef (IORef)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.UUID as U
import GHC.Records
import System.Console.Pretty (Color (..), color)
import Prelude hiding (log)

import Shared.Common.Constant.Component
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig hiding (email)
import Shared.Common.Model.Sentry.SentryEvent
import Shared.Common.Service.Sentry.SentryService
import Shared.Common.Util.Sentry
import Shared.Common.Util.String (f')

-- ---------------------------------------------------------------------------
logDebugI :: (MonadReader s m, MonadIO m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, HasField "breadcrumbs'" s (IORef [SentryBreadcrumb]), MonadLogger m) => String -> String -> m ()
logDebugI = logI LevelDebug

logInfoI :: (MonadReader s m, MonadIO m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, HasField "breadcrumbs'" s (IORef [SentryBreadcrumb]), MonadLogger m) => String -> String -> m ()
logInfoI = logI LevelInfo

logWarnI :: (MonadReader s m, MonadIO m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, HasField "breadcrumbs'" s (IORef [SentryBreadcrumb]), MonadLogger m) => String -> String -> m ()
logWarnI = logI LevelWarn

logErrorI
  :: ( MonadReader s m
     , MonadLogger m
     , MonadIO m
     , HasField "identity'" s (Maybe String)
     , HasField "identityEmail'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "breadcrumbs'" s (IORef [SentryBreadcrumb])
     , HasField "serverConfig'" s sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => String
  -> String
  -> m ()
logErrorI component message = do
  logI LevelError component message
  sendToSentry component message

logI :: (MonadReader s m, MonadIO m, HasField "identity'" s (Maybe String), HasField "traceUuid'" s U.UUID, HasField "breadcrumbs'" s (IORef [SentryBreadcrumb]), MonadLogger m) => LogLevel -> String -> String -> m ()
logI logLevel component message = do
  context <- ask
  now <- liftIO getCurrentTime
  liftIO . addBreadcrumb context.breadcrumbs' $
    SentryBreadcrumb {timestamp = now, level = toBreadcrumbLevel logLevel, category = trimComponent component, message = message}
  let mTraceUuid = Just . U.toString $ context.traceUuid'
  let record = createLogRecord logLevel context.identity' mTraceUuid component message
  logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

-- ---------------------------------------------------------------------------
logDebug :: MonadLogger m => String -> String -> m ()
logDebug = log LevelDebug

logInfo :: MonadLogger m => String -> String -> m ()
logInfo = log LevelInfo

logWarn :: MonadLogger m => String -> String -> m ()
logWarn = log LevelWarn

logError :: MonadLogger m => String -> String -> m ()
logError = log LevelError

-- ---------------------------------------------------------------------------
runLogging :: MonadIO m => LogLevel -> LoggingT m a -> m a
runLogging level = runStdoutLoggingT . filterLogger (filterAppLogging level)

filterAppLogging :: LogLevel -> LogSource -> LogLevel -> Bool
filterAppLogging LevelDebug _ (LevelOther "Debug") = True
filterAppLogging LevelDebug _ (LevelOther "Info ") = True
filterAppLogging LevelDebug _ (LevelOther "Warn ") = True
filterAppLogging LevelDebug _ (LevelOther "Error") = True
filterAppLogging LevelInfo _ (LevelOther "Debug") = False
filterAppLogging LevelInfo _ (LevelOther "Info ") = True
filterAppLogging LevelInfo _ (LevelOther "Warn ") = True
filterAppLogging LevelInfo _ (LevelOther "Error") = True
filterAppLogging LevelWarn _ (LevelOther "Debug") = False
filterAppLogging LevelWarn _ (LevelOther "Info ") = False
filterAppLogging LevelWarn _ (LevelOther "Warn ") = True
filterAppLogging LevelWarn _ (LevelOther "Error") = True
filterAppLogging LevelError _ (LevelOther "Debug") = False
filterAppLogging LevelError _ (LevelOther "Info ") = False
filterAppLogging LevelError _ (LevelOther "Warn ") = False
filterAppLogging LevelError _ (LevelOther "Error") = True
filterAppLogging _ _ _ = False

-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
log logLevel component message =
  let record = createLogRecord logLevel Nothing Nothing component message
   in logWithoutLoc "" (LevelOther . T.pack . showLogLevel $ logLevel) record

-- ---------------------------------------------------------------------------
createLogRecord :: LogLevel -> Maybe String -> Maybe String -> String -> String -> String
createLogRecord logLevel mUserUuid mTraceUuid component message = color recordColor record
  where
    userUuidStamp = createLoggerStamp "I" (fromMaybe "------------------------------------" mUserUuid)
    traceUuidStamp = createLoggerStamp "T" (fromMaybe "------------------------------------" mTraceUuid)
    componentStamp = createLoggerStamp "" component
    record = L.intercalate "" [userUuidStamp, " ", traceUuidStamp, " ", componentStamp, " ", message]
    recordColor = getColor logLevel

createLoggerStamp :: String -> String -> String
createLoggerStamp "" value = "[" ++ value ++ "]"
createLoggerStamp label value = "[" ++ label ++ ":" ++ value ++ "]"

getColor :: LogLevel -> Color
getColor LevelDebug = Default
getColor LevelInfo = Default
getColor LevelWarn = Magenta
getColor LevelError = Red
getColor (LevelOther _) = Default

toBreadcrumbLevel :: LogLevel -> String
toBreadcrumbLevel LevelDebug = "debug"
toBreadcrumbLevel LevelInfo = "info"
toBreadcrumbLevel LevelWarn = "warning"
toBreadcrumbLevel LevelError = "error"
toBreadcrumbLevel (LevelOther _) = "info"

trimComponent :: String -> String
trimComponent = T.unpack . T.strip . T.pack

showLogLevel :: LogLevel -> String
showLogLevel LevelDebug = "Debug"
showLogLevel LevelInfo = "Info "
showLogLevel LevelWarn = "Warn "
showLogLevel LevelError = "Error"
showLogLevel (LevelOther level) = T.unpack level

-- ---------------------------------------------------------------------------

sendToSentry
  :: ( MonadReader s m
     , MonadLogger m
     , MonadIO m
     , HasField "identity'" s (Maybe String)
     , HasField "identityEmail'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "breadcrumbs'" s (IORef [SentryBreadcrumb])
     , HasField "serverConfig'" s sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" s BuildInfoConfig
     )
  => String
  -> String
  -> m ()
sendToSentry component message = do
  context <- ask
  let traceUuid = U.toString context.traceUuid'
  captureAppSentryEvent
    (toSentryEvent "messageLogger" (normalizeMessage message))
      { fingerprint = [component, normalizeMessage message]
      , tags = [("component", trimComponent component), ("traceUuid", traceUuid)]
      , extra = [("message", String . T.pack $ message)]
      , interfaces = toUserInterface context.identity' context.identityEmail' ++ toTraceInterface traceUuid
      }
