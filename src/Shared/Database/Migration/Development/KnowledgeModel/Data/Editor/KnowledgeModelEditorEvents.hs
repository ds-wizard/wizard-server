module Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents where

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.Project.Data.ProjectReplies
import Shared.Util.Uuid

knowledgeModelEditorWebsocketEvent1' :: KnowledgeModelEditorWebSocketEventDTO
knowledgeModelEditorWebsocketEvent1' = AddKnowledgeModelEditorWebSocketEventDTO' knowledgeModelEditorWebsocketEvent1

knowledgeModelEditorWebsocketEvent1 :: AddKnowledgeModelEditorWebSocketEventDTO
knowledgeModelEditorWebsocketEvent1 =
  AddKnowledgeModelEditorWebSocketEventDTO
    { uuid = u' "6858b0b6-bb6f-4e21-a0c2-6afc84950f7a"
    , event = a_km1
    }

setRepliesDTO :: SetRepliesDTO
setRepliesDTO =
  SetRepliesDTO
    { uuid = u' "91863e00-ae98-4bcf-aae6-03a5a801b4fa"
    , replies = fReplies
    }
