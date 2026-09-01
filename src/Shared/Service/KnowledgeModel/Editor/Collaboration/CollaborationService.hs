module Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Cache.KnowledgeModelEditorWebsocketCache
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO (insertKnowledgeModelEvent)
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Websocket.WebsocketMessage
import Shared.Model.Websocket.WebsocketRecord
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationAcl
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper
import Shared.Service.Websocket.WebsocketService
import Shared.Util.Uuid
import Shared.Util.Websocket

putUserOnline :: WizardRequestContextC s m => U.UUID -> U.UUID -> Connection -> m ()
putUserOnline kmEditorUuid connectionUuid connection = do
  myself <- createRecord connectionUuid connection (U.toString kmEditorUuid) EditorWebsocketPerm []
  checkViewPermission myself
  _ <- findKnowledgeModelEditorByUuid kmEditorUuid
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList kmEditorUuid connectionUuid

deleteUser :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
deleteUser kmEditorUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList kmEditorUuid connectionUuid

setUserList :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
setUserList kmEditorUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged :: WizardRequestContextC s m => U.UUID -> m ()
logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditorUuid = do
  records <- getAllFromCache
  let error = NotExistsError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__COLLABORATION__FORCE_DISCONNECT (U.toString kmEditorUuid)
  traverse_ (logOut error) records
  where
    logOut error record =
      when
        (record.entityId == U.toString kmEditorUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: WizardRequestContextC s m => U.UUID -> U.UUID -> KnowledgeModelEditorWebSocketEventDTO -> m ()
setContent kmEditorUuid connectionUuid reqDto =
  case reqDto of
    AddKnowledgeModelEditorWebSocketEventDTO' event -> addKnowledgeModelEvent kmEditorUuid connectionUuid event

addKnowledgeModelEvent :: WizardRequestContextC s m => U.UUID -> U.UUID -> AddKnowledgeModelEditorWebSocketEventDTO -> m ()
addKnowledgeModelEvent kmEditorUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  tenantUuid <- asks (.tenantUuid')
  let kmEditorEvent = fromAddKnowledgeModelEditorWebSocketEventDTO reqDto.event kmEditorUuid tenantUuid
  insertKnowledgeModelEvent kmEditorEvent
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toAddKnowledgeModelEditorWebsocketMessage reqDto) disconnectUser

-- --------------------------------
setReplies :: WizardRequestContextC s m => U.UUID -> U.UUID -> SetRepliesDTO -> m ()
setReplies kmEditorUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  tenantUuid <- asks (.tenantUuid')
  let kmEditorReplies = fromSetRepliesEventDTO kmEditorUuid tenantUuid reqDto.replies
  updateKnowledgeModelRepliesByEditorUuid kmEditorUuid kmEditorReplies
  records <- getAllFromCache
  broadcast (U.toString kmEditorUuid) records (toSetRepliesMessage reqDto) disconnectUser

-- --------------------------------
disconnectUser :: (WizardRequestContextC s m, ToJSON resDto) => WebsocketMessage resDto -> m ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid
