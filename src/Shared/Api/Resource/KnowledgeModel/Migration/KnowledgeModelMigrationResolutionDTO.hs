module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration

data KnowledgeModelMigrationResolutionDTO = KnowledgeModelMigrationResolutionDTO
  { originalEventUuid :: U.UUID
  , action :: KnowledgeModelMigrationAction
  }
  deriving (Show, Eq, Generic)
