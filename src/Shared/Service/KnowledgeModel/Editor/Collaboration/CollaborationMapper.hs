module Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationMapper where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.Project.ProjectReply

toAddKnowledgeModelEditorWebsocketMessage :: AddKnowledgeModelEditorWebSocketEventDTO -> Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO
toAddKnowledgeModelEditorWebsocketMessage = Success_ServerActionDTO . SetContent_ServerKnowledgeModelEditorMessageDTO . AddKnowledgeModelEditorWebSocketEventDTO'

toSetRepliesMessage :: SetRepliesDTO -> Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO
toSetRepliesMessage = Success_ServerActionDTO . SetReplies_ServerKnowledgeModelEditorMessageDTO

toAddKnowledgeModelEditorWebSocketEventDTO :: KnowledgeModelEvent -> AddKnowledgeModelEditorWebSocketEventDTO
toAddKnowledgeModelEditorWebSocketEventDTO event =
  AddKnowledgeModelEditorWebSocketEventDTO
    { uuid = event.uuid
    , event = event
    }

fromAddKnowledgeModelEditorWebSocketEventDTO :: KnowledgeModelEvent -> U.UUID -> U.UUID -> KnowledgeModelEditorEvent
fromAddKnowledgeModelEditorWebSocketEventDTO KnowledgeModelEvent {..} knowledgeModelEditorUuid tenantUuid = KnowledgeModelEditorEvent {..}

fromSetRepliesEventDTO :: U.UUID -> U.UUID -> M.Map String Reply -> [KnowledgeModelEditorReply]
fromSetRepliesEventDTO knowledgeModelEditorUuid tenantUuid replies = fmap (fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid) (M.toList replies)

fromSetReplyEventDTO :: U.UUID -> U.UUID -> (String, Reply) -> KnowledgeModelEditorReply
fromSetReplyEventDTO knowledgeModelEditorUuid tenantUuid (path, Reply {..}) = KnowledgeModelEditorReply {..}
