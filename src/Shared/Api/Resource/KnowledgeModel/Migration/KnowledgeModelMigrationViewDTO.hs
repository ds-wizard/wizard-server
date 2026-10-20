module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

data KnowledgeModelMigrationViewDTO = KnowledgeModelMigrationViewDTO
  { editorUuid :: U.UUID
  , state :: String
  , editorPreviousPackage :: KnowledgeModelPackageSuggestion
  , targetPackage :: KnowledgeModelPackageSuggestion
  , processedEvents :: Int
  , totalEvents :: Int
  , conflict :: Maybe KnowledgeModelMigrationConflictDTO
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelMigrationConflictDTO = KnowledgeModelMigrationConflictDTO
  { eventUuid :: U.UUID
  , event :: KnowledgeModelEditorEventChangeDTO
  , before :: [KnowledgeModelEntityNodeDTO]
  , after :: [KnowledgeModelEntityNodeDTO]
  }
  deriving (Show, Eq, Generic)
