module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO where

import Data.Aeson (Value)
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditorEventsChangeDTO = KnowledgeModelEditorEventsChangeDTO
  { events :: [KnowledgeModelEditorEventChangeDTO]
  , baseRevision :: Maybe U.UUID
  , dryRun :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelEditorEventChangeDTO = KnowledgeModelEditorEventChangeDTO
  { parentUuid :: String
  , entityUuid :: String
  , content :: Value
  , position :: Maybe Int
  }
  deriving (Show, Eq, Generic)
