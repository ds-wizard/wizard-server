module Specs.Api.Handler.Websocket.Common where

import Control.Concurrent.Async
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Network.Socket
import Network.WebSockets hiding (serverPort)

import Shared.Model.Config.WizardServerConfig
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketNodeService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Util.Uuid
import Shared.Worker.WebsocketWorkers
import WizardServer.Application
import WizardServer.Model.Context.ContextMappers
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Websocket

runWebserver requestContext action = do
  result <- race (runWebServer (serverContextFromRequestContext requestContext)) (waitForNode requestContext 1 >> action)
  either (const (fail "The web server stopped")) return result

runWebsocketNode :: RequestContext -> IO a -> IO a
runWebsocketNode requestContext action =
  withAsync (websocketBusListenerJob (runRequestContextWithServerContext runWebsocketBusListener serverContext) serverContext) $ \_ ->
    withAsync (websocketHeartbeatJob (runRequestContextWithServerContext'' refreshNodeHeartbeat serverContext) serverContext) $ \_ ->
      runWebserver requestContext action
  where
    serverContext = serverContextFromRequestContext requestContext

withSecondNode :: RequestContext -> (RequestContext -> IO a) -> IO a
withSecondNode requestContext action = do
  websocketRegistry <- createWebsocketRegistry
  nodeUuid <- generateUuid
  let general = requestContext.serverConfig.general
  let serverConfig = requestContext.serverConfig {general = general {serverPort = general.serverPort + 1}}
  let requestContextB = requestContext {serverConfig = serverConfig, websocketRegistry = websocketRegistry, nodeUuid = nodeUuid}
  runWebsocketNode requestContextB (action requestContextB)

waitForNode :: RequestContext -> Int -> IO ()
waitForNode requestContext = waitForWebsocketNode requestContext.serverConfig.general.serverPort requestContext.websocketRegistry

createConnection requestContext = createConnectionOnPort requestContext.serverConfig.general.serverPort

closeConnection connection = sendClose connection (T.pack "close")

closeSockets = traverse_ close

sendMessage connection content = sendTextData connection (encode content)
