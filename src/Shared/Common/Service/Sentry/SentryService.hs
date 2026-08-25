module Shared.Common.Service.Sentry.SentryService where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.IORef (IORef, readIORef)
import GHC.Records

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
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
