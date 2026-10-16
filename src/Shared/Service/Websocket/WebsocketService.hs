module Shared.Service.Websocket.WebsocketService where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value, encode)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketConnection
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Service.User.WizardUserMapper
import Shared.Service.Websocket.WebsocketMapper
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Util.Number
import Shared.Util.Websocket

connectUser :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> WebsocketOutbound -> WebsocketPerm -> [U.UUID] -> [String] -> m ()
connectUser key@(tenantUuid, subscription, entityUuid) connectionUuid outbound websocketPerm userGroupUuids tenantRolePermissions = do
  mCurrentUser <- asks (.currentUser')
  nodeUuid <- asks (.nodeUuid')
  avatarNumber <- liftIO $ generateInt 20
  colorNumber <- liftIO $ generateInt 12
  let onlineUser = toOnlineUserInfo mCurrentUser avatarNumber colorNumber userGroupUuids
  let userUuid = fmap (.uuid) mCurrentUser
  insertWebsocketConnection WebsocketConnection {uuid = connectionUuid, ..}
  addLocalConnection key LocalWebsocketConnection {..}

disconnectUser :: WizardRequestContextC s m => WebsocketEntityKey -> U.UUID -> m Bool
disconnectUser key connectionUuid = do
  present <- removeLocalConnection key connectionUuid
  deleteWebsocketConnectionByUuid connectionUuid
  return present

disconnectUserWithError :: WizardRequestContextC s m => WebsocketEntityKey -> LocalWebsocketConnection -> AppError -> m ()
disconnectUserWithError key connection error = do
  disconnectUser key connection.connectionUuid
  closeConnection connection 1008 (Just error)

broadcastLocal :: WizardRequestContextC s m => WebsocketEntityKey -> WebsocketRecipients -> Value -> m ()
broadcastLocal key recipients payload = do
  connections <- findLocalConnections key
  let encoded = encode payload
  traverse_ (\connection -> enqueueOutbound connection.outbound connection.connectionUuid encoded) (filter (matchesRecipients recipients) connections)

sendUserList :: WizardRequestContextC s m => WebsocketEntityKey -> m ()
sendUserList key@(tenantUuid, subscription, entityUuid) = do
  rows <- findWebsocketConnectionsByEntity tenantUuid subscription entityUuid
  connections <- findLocalConnections key
  traverse_ (sendTo rows) connections
  where
    sendTo rows connection =
      let others = fmap (.onlineUser) . filter ((/= connection.connectionUuid) . (.uuid)) $ rows
       in traverse_ (enqueueMessage connection) (toUserListMessage subscription others)

logOutAll :: WizardRequestContextC s m => WebsocketEntityKey -> m ()
logOutAll key@(_, subscription, entityUuid) = do
  connections <- findLocalConnections key
  traverse_ (\error -> traverse_ (\connection -> disconnectUserWithError key connection error) connections) (toForceDisconnectError subscription entityUuid)
