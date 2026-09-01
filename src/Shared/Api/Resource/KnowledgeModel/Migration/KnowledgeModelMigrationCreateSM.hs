module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Util.Swagger

instance ToSchema KnowledgeModelMigrationCreateDTO where
  declareNamedSchema = toSwagger knowledgeModelMigrationCreateDTO
