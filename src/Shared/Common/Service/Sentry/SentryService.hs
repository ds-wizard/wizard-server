module Shared.Common.Service.Sentry.SentryService where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.IORef (IORef, readIORef)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Model.Sentry.SentryEvent
import Shared.Common.Util.Sentry

toSentryTarget
  :: ( HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     )
  => context
  -> SentryTarget
toSentryTarget context =
  SentryTarget
    { enabled = context.serverConfig'.sentry'.enabled
    , dsn = context.serverConfig'.sentry'.dsn
    , environment = context.serverConfig'.sentry'.environment
    , release = context.buildInfoConfig'.version
    }

captureSentryEvent
  :: ( MonadReader context m
     , MonadIO m
     , HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     )
  => SentryEvent
  -> m ()
captureSentryEvent event = do
  context <- ask
  liftIO $ sendSentryEvent (toSentryTarget context) event

captureAppSentryEvent
  :: ( MonadReader context m
     , MonadIO m
     , HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     , HasField "breadcrumbs'" context (IORef [SentryBreadcrumb])
     )
  => SentryEvent
  -> m ()
captureAppSentryEvent event = do
  context <- ask
  collected <- liftIO . readIORef $ context.breadcrumbs'
  captureSentryEvent event {breadcrumbs = reverse collected}

guardAppContext
  :: ( HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     , HasField "breadcrumbs'" context (IORef [SentryBreadcrumb])
     , HasField "identity'" context (Maybe String)
     , HasField "traceUuid'" context U.UUID
     , HasField "tenantUuid'" context U.UUID
     )
  => context
  -> IO (Either AppError a)
  -> IO (Either AppError a)
guardAppContext context action = do
  result <- try action
  case result of
    Right (Left (GeneralServerError message)) -> do
      sendAppSentryEvent context (toGeneralServerErrorEvent message)
      return . Left . GeneralServerError $ message
    Right outcome -> return outcome
    Left exception -> rethrowWithAppContext context exception

rethrowWithAppContext
  :: ( HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     , HasField "breadcrumbs'" context (IORef [SentryBreadcrumb])
     , HasField "identity'" context (Maybe String)
     , HasField "traceUuid'" context U.UUID
     , HasField "tenantUuid'" context U.UUID
     )
  => context
  -> SomeException
  -> IO a
rethrowWithAppContext context exception
  | isIgnoredException exception = throwIO exception
  | otherwise = do
      collected <- readIORef context.breadcrumbs'
      throwWithAppContext
        AppContextException
          { appException = exception
          , appBreadcrumbs = reverse collected
          , appIdentity = context.identity'
          , appTraceUuid = U.toString context.traceUuid'
          , appTenantUuid = U.toString context.tenantUuid'
          }

sendAppSentryEvent
  :: ( HasField "serverConfig'" context sc
     , HasField "sentry'" sc ServerConfigSentry
     , HasField "buildInfoConfig'" context BuildInfoConfig
     , HasField "breadcrumbs'" context (IORef [SentryBreadcrumb])
     , HasField "identity'" context (Maybe String)
     , HasField "traceUuid'" context U.UUID
     , HasField "tenantUuid'" context U.UUID
     )
  => context
  -> SentryEvent
  -> IO ()
sendAppSentryEvent context event = do
  collected <- readIORef context.breadcrumbs'
  sendSentryEvent (toSentryTarget context) (enrichWithContext context event {breadcrumbs = reverse collected})

enrichWithContext
  :: ( HasField "identity'" context (Maybe String)
     , HasField "traceUuid'" context U.UUID
     , HasField "tenantUuid'" context U.UUID
     )
  => context
  -> SentryEvent
  -> SentryEvent
enrichWithContext context event =
  let traceUuid = U.toString context.traceUuid'
   in event
        { tags = event.tags ++ [("traceUuid", traceUuid), ("tenantUuid", U.toString context.tenantUuid')]
        , interfaces = event.interfaces ++ toUserInterface context.identity' Nothing ++ toTraceInterface traceUuid
        }
