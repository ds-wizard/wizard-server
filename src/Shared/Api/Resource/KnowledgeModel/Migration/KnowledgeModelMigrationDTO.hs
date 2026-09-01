module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

data KnowledgeModelMigrationDTO = KnowledgeModelMigrationDTO
  { editorUuid :: U.UUID
  , editorName :: String
  , editorPreviousPackage :: KnowledgeModelPackageSuggestion
  , state :: KnowledgeModelMigrationState
  , targetPackage :: KnowledgeModelPackageSuggestion
  , currentKnowledgeModel :: Maybe KnowledgeModel
  }
  deriving (Show, Eq, Generic)
