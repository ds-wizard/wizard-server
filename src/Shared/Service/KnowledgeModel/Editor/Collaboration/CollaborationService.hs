module Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService where

import Control.Monad (void, when)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO (insertKnowledgeModelEvent)
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper
import Shared.Service.Websocket.WebsocketBusService
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Util.Websocket

putUserOnline :: WizardRequestContextC s m => U.UUID -> U.UUID -> WebsocketOutbound -> m ()
putUserOnline kmEditorUuid connectionUuid outbound = do
  checkViewPermission kmEditorUuid
  _ <- findKnowledgeModelEditorByUuid kmEditorUuid
  key <- editorKey kmEditorUuid
  connectUser key connectionUuid outbound EditorWebsocketPerm [] []
  logWS connectionUuid "New user added to the list"
  publishUserListChanged key

deleteUser :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteUser kmEditorUuid connectionUuid = do
  key <- editorKey kmEditorUuid
  present <- disconnectUser key connectionUuid
  when present (publishUserListChanged key)

logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged :: WizardRequestContextC s m => U.UUID -> m ()
logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid = do
  (tenantUuid, subscription, entityUuid) <- editorKey kmEditorUuid
  publish LogOutAll {..}

-- --------------------------------
setContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> KnowledgeModelEditorWebSocketEventDTO -> m ()
setContent kmEditorUuid connectionUuid reqDto =
  case reqDto of
    AddKnowledgeModelEditorWebSocketEventDTO' event -> addKnowledgeModelEvent kmEditorUuid connectionUuid event

addKnowledgeModelEvent :: WizardRequestContextC s m => U.UUID -> U.UUID -> AddKnowledgeModelEditorWebSocketEventDTO -> m ()
addKnowledgeModelEvent kmEditorUuid connectionUuid reqDto = do
  checkConnection kmEditorUuid connectionUuid
  checkEditPermission kmEditorUuid
  tenantUuid <- asks (.tenantUuid')
  insertKnowledgeModelEvent (fromAddKnowledgeModelEditorWebSocketEventDTO reqDto.event kmEditorUuid tenantUuid)
  updateKnowledgeModelEditorSquashedByUuid kmEditorUuid False
  broadcast kmEditorUuid (toAddKnowledgeModelEditorWebsocketMessage reqDto)

-- --------------------------------
setReplies :: WizardRequestContextC s m => U.UUID -> U.UUID -> SetRepliesDTO -> m ()
setReplies kmEditorUuid connectionUuid reqDto = do
  checkConnection kmEditorUuid connectionUuid
  checkEditPermission kmEditorUuid
  tenantUuid <- asks (.tenantUuid')
  updateKnowledgeModelRepliesByEditorUuid kmEditorUuid (fromSetRepliesEventDTO kmEditorUuid tenantUuid reqDto.replies)
  broadcast kmEditorUuid (toSetRepliesMessage reqDto)

-- --------------------------------
-- PRIVATE
-- --------------------------------
editorKey :: WizardRequestContextC s m => U.UUID -> m WebsocketEntityKey
editorKey kmEditorUuid = do
  tenantUuid <- asks (.tenantUuid')
  return (tenantUuid, KnowledgeModelEditorWebsocketSubscription, kmEditorUuid)

checkConnection :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
checkConnection kmEditorUuid connectionUuid = do
  key <- editorKey kmEditorUuid
  void $ findLocalConnection' key connectionUuid

broadcast :: (WizardRequestContextC s m, ToJSON content) => U.UUID -> content -> m ()
broadcast kmEditorUuid content = do
  (tenantUuid, subscription, entityUuid) <- editorKey kmEditorUuid
  publish Broadcast {recipients = AllRecipients, payload = toJSON content, ..}
