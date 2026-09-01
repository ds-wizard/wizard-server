module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationActionJM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelMigrationResolutionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationResolutionDTO where
  toJSON = genericToJSON jsonOptions
