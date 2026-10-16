module Shared.Service.Websocket.WebsocketNodeService where

import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (traverse_)
import qualified Data.Set as S
import System.Timeout (timeout)

import Shared.Constant.Websocket
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Util.Websocket

refreshNodeHeartbeat :: WizardRequestContextC s m => m ()
refreshNodeHeartbeat = do
  nodeUuid <- asks (.nodeUuid')
  connections <- findAllLocalConnections
  refreshed <- S.fromList <$> updateWebsocketConnectionHeartbeat nodeUuid (fmap ((.connectionUuid) . snd) connections)
  let purged = filter ((`S.notMember` refreshed) . (.connectionUuid) . snd) connections
  traverse_ (\(_, connection) -> closeConnection connection 1012 Nothing) purged

purgeStaleWebsocketConnections :: WizardRequestContextC s m => m ()
purgeStaleWebsocketConnections = deleteStaleWebsocketConnections >>= traverse_ publishUserListChanged . nubOrd

shutdownWebsocketNode :: WizardRequestContextC s m => m ()
shutdownWebsocketNode = do
  connections <- findAllLocalConnections
  removeLocalConnections connections
  asks (.nodeUuid') >>= void . deleteWebsocketConnectionsByNodeUuid
  traverse_ publishUserListChanged . nubOrd . fmap fst $ connections
  traverse_ (\(_, connection) -> closeConnection connection 1001 Nothing) connections
  liftIO . void . timeout websocketShutdownDrainMicros $ traverse_ (\(_, connection) -> atomically (readTMVar connection.outbound.finished)) connections
