module Shared.Api.Resource.Project.Migration.ProjectMigrationResultDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

data ProjectMigrationResultDTO = ProjectMigrationResultDTO
  { uuid :: U.UUID
  , name :: String
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , selectedQuestionTagUuids :: [U.UUID]
  , phaseUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
