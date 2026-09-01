module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorWebSocketEventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema AddKnowledgeModelEditorWebSocketEventDTO where
  declareNamedSchema = toSwagger knowledgeModelEditorWebsocketEvent1'
