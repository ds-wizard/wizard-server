module WizardServer.Worker.PermanentWorkers where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (liftIO)
import Prelude hiding (log)

import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketNodeService
import Shared.Util.Logger
import Shared.Worker.WebsocketWorkers
import WizardServer.Model.Context.ContextMappers
import WizardServer.Model.Context.ServerContext

permanentWorker :: ServerContext -> LoggingT IO [ThreadId]
permanentWorker context = do
  threadId <- liftIO $ forkIO (persistentCommandListenerJob context)
  websocketBusThreadId <- liftIO $ forkIO (websocketBusListenerJob (runRequestContextWithServerContext runWebsocketBusListener context) context)
  websocketHeartbeatThreadId <- liftIO $ forkIO (websocketHeartbeatJob (runRequestContextWithServerContext'' refreshNodeHeartbeat context) context)
  return [threadId, websocketBusThreadId, websocketHeartbeatThreadId]

-- -----------------------------------------------------------------------------
-- WORKERS
-- -----------------------------------------------------------------------------
persistentCommandListenerJob :: ServerContext -> IO ()
persistentCommandListenerJob context =
  when
    context.serverConfig.persistentCommand.listenerJob.enabled
    ( do
        let loggingLevel = context.serverConfig.logging.level
         in runLogging loggingLevel $ do
              logInfo _CMP_WORKER "PersistentCommandWorker: starting"
              liftIO $ runRequestContextWithServerContext runPersistentCommandChannelListener' context
              logInfo _CMP_WORKER "PersistentCommandWorker: ended"
    )
