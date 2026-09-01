module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationJM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Util.Swagger

instance ToSchema KnowledgeModelMigrationDTO where
  declareNamedSchema = toSwagger knowledgeModelMigrationDTO
