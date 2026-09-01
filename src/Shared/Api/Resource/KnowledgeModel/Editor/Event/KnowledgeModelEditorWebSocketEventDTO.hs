module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent

data KnowledgeModelEditorWebSocketEventDTO
  = AddKnowledgeModelEditorWebSocketEventDTO' AddKnowledgeModelEditorWebSocketEventDTO
  deriving (Show, Eq, Generic)

data AddKnowledgeModelEditorWebSocketEventDTO = AddKnowledgeModelEditorWebSocketEventDTO
  { uuid :: U.UUID
  , event :: KnowledgeModelEvent
  }
  deriving (Show, Eq, Generic)
