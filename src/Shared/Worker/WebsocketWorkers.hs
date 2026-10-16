module Shared.Worker.WebsocketWorkers where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (liftIO)
import Data.Either (fromLeft)

import Shared.Constant.Websocket
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardServerContext
import Shared.Util.Exception
import Shared.Util.Logger

websocketBusListenerJob :: WizardServerContextType s => IO (Either String ()) -> s -> IO ()
websocketBusListenerJob listener context =
  runLogging context.serverConfig'.logging.level . forever $ do
    logInfo _CMP_WORKER "WebsocketBusListener: starting"
    result <- liftIO (tryNonAsync listener)
    logWarn _CMP_WORKER (f' "WebsocketBusListener: stopped (%s), restarting" [either show (fromLeft "ended") result])
    liftIO (threadDelay websocketListenerRestartDelayMicros)

websocketHeartbeatJob :: WizardServerContextType s => IO (Either String ()) -> s -> IO ()
websocketHeartbeatJob heartbeat context =
  runLogging context.serverConfig'.logging.level . forever $ do
    liftIO (threadDelay websocketHeartbeatMicros)
    result <- liftIO (tryNonAsync heartbeat)
    either (logWarn _CMP_WORKER . f' "WebsocketHeartbeat: failed (%s)" . pure . show) (const (return ())) result
