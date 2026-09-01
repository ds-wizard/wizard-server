module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Util.Swagger

instance ToSchema KnowledgeModelMigrationState where
  declareNamedSchema = toSwaggerWithFlatType "type" RunningKnowledgeModelMigrationState
