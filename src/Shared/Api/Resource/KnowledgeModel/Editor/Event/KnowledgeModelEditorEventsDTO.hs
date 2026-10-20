module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO

data KnowledgeModelEditorEventsDTO = KnowledgeModelEditorEventsDTO
  { revision :: Maybe U.UUID
  , names :: M.Map String U.UUID
  , outline :: [KnowledgeModelEntityNodeDTO]
  }
  deriving (Show, Eq, Generic)
