module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationActionSM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Util.Swagger

instance ToSchema KnowledgeModelMigrationResolutionDTO where
  declareNamedSchema = toSwagger knowledgeModelMigrationResolutionDTO
