module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationDTO where
  toJSON = genericToJSON jsonOptions
