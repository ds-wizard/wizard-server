module Shared.Service.Websocket.WebsocketRegistryService where

import Control.Concurrent.STM
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.UUID as U

import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription

createWebsocketRegistry :: IO WebsocketRegistry
createWebsocketRegistry = WebsocketRegistry <$> newTVarIO M.empty <*> newTVarIO S.empty <*> newTVarIO 0

addLocalConnection :: WizardRequestContextC s m => WebsocketEntityKey -> LocalWebsocketConnection -> m ()
addLocalConnection key connection = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically . modifyTVar' registry.connections $
    M.insertWith M.union key (M.singleton connection.connectionUuid connection)

adjustLocalConnection :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> (LocalWebsocketConnection -> LocalWebsocketConnection) -> m ()
adjustLocalConnection key connectionUuid update = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically . modifyTVar' registry.connections $ M.adjust (M.adjust update connectionUuid) key

removeLocalConnection :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> m Bool
removeLocalConnection key connectionUuid = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically $ do
    connections <- readTVar registry.connections
    let present = M.member connectionUuid (fromMaybe M.empty (M.lookup key connections))
    writeTVar registry.connections (M.update (nonEmpty . M.delete connectionUuid) key connections)
    return present
  where
    nonEmpty entityConnections = if M.null entityConnections then Nothing else Just entityConnections

findLocalConnections :: WizardRequestContextC s m => WebsocketEntityKey -> m [LocalWebsocketConnection]
findLocalConnections key = do
  registry <- asks (.websocketRegistry')
  connections <- liftIO $ readTVarIO registry.connections
  return . M.elems . fromMaybe M.empty $ M.lookup key connections

findLocalConnection :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> m (Maybe LocalWebsocketConnection)
findLocalConnection key connectionUuid = do
  registry <- asks (.websocketRegistry')
  connections <- liftIO $ readTVarIO registry.connections
  return $ M.lookup key connections >>= M.lookup connectionUuid

findLocalConnection' :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> m LocalWebsocketConnection
findLocalConnection' key connectionUuid = do
  mConnection <- findLocalConnection key connectionUuid
  case mConnection of
    Just connection -> return connection
    Nothing -> throwError . NotExistsError . _ERROR_API__WEBSOCKET_RECORD_NOT_FOUND $ U.toString connectionUuid

findLocalEntityUuids :: WizardRequestContextC s m => U.UUID -> WebsocketSubscription -> m [U.UUID]
findLocalEntityUuids tenantUuid subscription = do
  registry <- asks (.websocketRegistry')
  connections <- liftIO $ readTVarIO registry.connections
  return [entityUuid | (keyTenantUuid, keySubscription, entityUuid) <- M.keys connections, keyTenantUuid == tenantUuid, keySubscription == subscription]

countLocalConnectionsByEntity :: WizardRequestContextC s m => m [(WebsocketEntityKey, Int)]
countLocalConnectionsByEntity = do
  registry <- asks (.websocketRegistry')
  connections <- liftIO $ readTVarIO registry.connections
  return . M.toList . M.map M.size $ connections

markUserListPending :: WizardRequestContextC s m => WebsocketEntityKey -> m Bool
markUserListPending key = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically $ do
    pending <- readTVar registry.pendingUserLists
    writeTVar registry.pendingUserLists (S.insert key pending)
    return (S.notMember key pending)

clearUserListPending :: WizardRequestContextC s m => WebsocketEntityKey -> m ()
clearUserListPending key = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically $ modifyTVar' registry.pendingUserLists (S.delete key)

findAllLocalConnections :: WizardRequestContextC s m => m [(WebsocketEntityKey, LocalWebsocketConnection)]
findAllLocalConnections = do
  registry <- asks (.websocketRegistry')
  connections <- liftIO $ readTVarIO registry.connections
  return [(key, connection) | (key, entityConnections) <- M.toList connections, connection <- M.elems entityConnections]

removeLocalConnections :: WizardRequestContextC s m => [(WebsocketEntityKey, LocalWebsocketConnection)] -> m ()
removeLocalConnections = traverse_ (\(key, connection) -> removeLocalConnection key connection.connectionUuid)

markListening :: WizardRequestContextC s m => m Int
markListening = do
  registry <- asks (.websocketRegistry')
  liftIO . atomically $ stateTVar registry.listenCount (\count -> (count, count + 1))
