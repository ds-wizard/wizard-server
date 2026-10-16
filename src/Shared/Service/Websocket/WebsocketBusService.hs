module Shared.Service.Websocket.WebsocketBusService where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Reader (asks, liftIO, local)
import Data.Aeson (Result (..), eitherDecodeStrict, encode, fromJSON, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple.Notification

import Shared.Constant.Websocket
import Shared.Database.DAO.Websocket.WebsocketBusDAO
import Shared.Database.DAO.Websocket.WebsocketOutboxDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketOutbox
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.Project.Collaboration.ProjectCollaborationPermissionService
import Shared.Service.Websocket.WebsocketMapper
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Util.Exception
import Shared.Util.Logger
import Shared.Util.Uuid
import Shared.Util.Websocket

publish :: WizardRequestContextC s m => WebsocketBusMessage -> m ()
publish message = do
  let encoded = encode message
  if BSL.length encoded <= websocketInlinePayloadLimit
    then notifyWebsocketChannel encoded
    else do
      uuid <- liftIO generateUuid
      insertWebsocketOutbox WebsocketOutbox {uuid = uuid, tenantUuid = message.tenantUuid, payload = toJSON message}
      notifyWebsocketChannel (encode (WebsocketBusOutbox uuid))

publishUserListChanged :: WizardRequestContextC s m => WebsocketEntityKey -> m ()
publishUserListChanged (tenantUuid, subscription, entityUuid) = publish UserListChanged {..}

purgeWebsocketOutbox :: WizardRequestContextC s m => m ()
purgeWebsocketOutbox = void deleteOldWebsocketOutboxes

runWebsocketBusListener :: WizardRequestContextC s m => m ()
runWebsocketBusListener = do
  listenWebsocketChannel
  previousListens <- markListening
  when (previousListens > 0) closeAfterListenerRestart
  forever (waitForWebsocketNotification websocketListenerLivenessMicros >>= traverse_ applyNotification)

closeAfterListenerRestart :: WizardRequestContextC s m => m ()
closeAfterListenerRestart = do
  connections <- findAllLocalConnections
  logInfoI _CMP_WORKER (f' "Websocket bus listener restarted, closing %s local connections" [show (length connections)])
  traverse_ (\(_, connection) -> closeConnection connection 1012 Nothing) connections

applyNotification :: forall s m. WizardRequestContextC s m => Notification -> m ()
applyNotification notification = do
  context <- asks (setDbConnection Nothing)
  result <- liftIO . tryNonAsync $ runRequestContextWithRequestContext @s @m (applyPayload (notificationData notification)) context
  either (logWarnI _CMP_WORKER . f' "Websocket bus message failed: %s" . pure . show) (const (return ())) result

applyPayload :: WizardRequestContextC s m => BS.ByteString -> m ()
applyPayload payload =
  case eitherDecodeStrict payload of
    Right envelope -> applyEnvelope envelope
    Left error -> logWarnI _CMP_WORKER ("Websocket bus message could not be decoded: " ++ error)

applyEnvelope :: WizardRequestContextC s m => WebsocketBusEnvelope -> m ()
applyEnvelope (WebsocketBusInline message) = apply message
applyEnvelope (WebsocketBusOutbox uuid) = do
  mOutbox <- findWebsocketOutboxByUuid' uuid
  traverse_ (applyOutbox uuid) mOutbox
  where
    applyOutbox uuid outbox =
      case fromJSON outbox.payload of
        Success message -> apply message
        Error error -> logWarnI _CMP_WORKER (f' "Websocket outbox '%s' could not be decoded: %s" [U.toString uuid, error])

apply :: WizardRequestContextC s m => WebsocketBusMessage -> m ()
apply message =
  local (setTenantUuid message.tenantUuid) $
    case message of
      Broadcast {..} -> broadcastLocal (tenantUuid, subscription, entityUuid) recipients payload
      UserListChanged {..} -> scheduleUserList (tenantUuid, subscription, entityUuid)
      ProjectPermissionsChanged {..} -> do
        removed <- updateProjectPermissions projectUuid
        when removed $ publishUserListChanged (tenantUuid, ProjectWebsocketSubscription, projectUuid)
      WorkspacePermissionsChanged {..} -> do
        projectUuids <- updateWorkspacePermissions workspaceUuid
        traverse_ (\projectUuid -> publishUserListChanged (tenantUuid, ProjectWebsocketSubscription, projectUuid)) projectUuids
      UserGroupRemoved {..} -> removeUserGroup userGroupUuid userUuids
      LogOutAll {..} -> logOutAll (tenantUuid, subscription, entityUuid)
      AnalyticsEtlFinished {..} ->
        broadcastLocal (tenantUuid, AnalyticsWebsocketSubscription, tenantUuid) AllRecipients toAnalyticsEtlFinishedMessage

scheduleUserList :: forall s m. WizardRequestContextC s m => WebsocketEntityKey -> m ()
scheduleUserList key = do
  connections <- findLocalConnections key
  unless (null connections) $ do
    first <- markUserListPending key
    when first $ do
      context <- asks (setDbConnection Nothing)
      void . liftIO . forkIO $ do
        threadDelay websocketUserListDebounceMicros
        result <- tryNonAsync $ runRequestContextWithRequestContext @s @m (clearUserListPending key >> sendUserList key) context
        either (void . flip (runRequestContextWithRequestContext @s @m) context . logUserListFailure) (const (return ())) result

logUserListFailure :: WizardRequestContextC s m => SomeException -> m ()
logUserListFailure error = logWarnI _CMP_WORKER (f' "Websocket user list failed: %s" [show error])
