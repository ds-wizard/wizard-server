module Shared.Util.Websocket where

import qualified Control.Exception.Base as E
import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection, sendTextData)

import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.OnlineUserInfo
import Shared.Model.Websocket.WebsocketMessage
import Shared.Model.Websocket.WebsocketRecord
import Shared.Util.Logger

-- --------------------------------
-- PRIVATE
-- --------------------------------
-- Websocket
broadcast
  :: (WizardRequestContextC s m, ToJSON a)
  => String
  -> [WebsocketRecord]
  -> (WebsocketRecord -> WebsocketMessage a)
  -> (WebsocketMessage a -> m ())
  -> m ()
broadcast entityUuid records toMessage disconnectUser =
  traverse_ (sendMessage disconnectUser . toMessage) (filter (filterByEntityId entityUuid) records)

sendMessage :: (WizardRequestContextC s m, ToJSON a) => (WebsocketMessage a -> m ()) -> WebsocketMessage a -> m ()
sendMessage disconnectUser msg = do
  logWS msg.connectionUuid "Sending message..."
  eResult <- liftIO $ E.try $ sendTextData msg.connection (encode msg.content)
  case eResult of
    Right _ -> do
      logWS msg.connectionUuid "Successfully sent"
      return ()
    Left (e :: E.SomeException) -> do
      logWS msg.connectionUuid "Failed to sent. Start disconnecting"
      disconnectUser msg
      logWS msg.connectionUuid "Successfully disconnected"
      return ()

sendError
  :: WizardRequestContextC s m
  => U.UUID
  -> Connection
  -> String
  -> (WebsocketMessage Error_ServerActionDTO -> m ())
  -> AppError
  -> m ()
sendError connectionUuid connection entityId disconnectUser error@ForbiddenError {} = do
  let msg = createErrorWebsocketMessage connectionUuid connection entityId error
  sendMessage disconnectUser msg
  disconnectUser msg
sendError connectionUuid connection entityId disconnectUser error =
  sendMessage disconnectUser $ createErrorWebsocketMessage connectionUuid connection entityId error

-- Filter
exceptMyself :: U.UUID -> WebsocketRecord -> Bool
exceptMyself myConnectionUuid record = record.connectionUuid /= myConnectionUuid

filterByEntityId :: String -> WebsocketRecord -> Bool
filterByEntityId projectUuid record = projectUuid == record.entityId

-- Accessors
getCollaborators :: U.UUID -> String -> [WebsocketRecord] -> [OnlineUserInfo]
getCollaborators connectionUuid entityId =
  fmap (.user) . filter (exceptMyself connectionUuid) . filter (filterByEntityId entityId)

-- Mapper
createErrorWebsocketMessage :: U.UUID -> Connection -> String -> AppError -> WebsocketMessage Error_ServerActionDTO
createErrorWebsocketMessage connectionUuid connection entityId error =
  WebsocketMessage
    { connectionUuid = connectionUuid
    , connection = connection
    , entityId = entityId
    , content = Error_ServerActionDTO error
    }

-- Logs
logWS :: WizardRequestContextC s m => U.UUID -> String -> m ()
logWS connectionUuid message = do
  serverConfig <- asks (.serverConfig')
  when
    serverConfig.logging.websocketDebug
    (logInfoI _CMP_SERVICE (f' "[C:%s] %s" [U.toString connectionUuid, message]))
