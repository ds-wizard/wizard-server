module Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesSM ()
import Shared.Api.Resource.User.OnlineUserInfoSM ()
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorMessages
import Shared.Util.Swagger

instance ToSchema ClientKnowledgeModelEditorMessageDTO where
  declareNamedSchema = toSwagger ensureOnlineUserAction

instance ToSchema ServerKnowledgeModelEditorMessageDTO where
  declareNamedSchema = toSwagger setUserListAction
