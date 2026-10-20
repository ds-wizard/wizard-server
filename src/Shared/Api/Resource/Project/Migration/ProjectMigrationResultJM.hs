module Shared.Api.Resource.Project.Migration.ProjectMigrationResultJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.Project.Migration.ProjectMigrationResultDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON ProjectMigrationResultDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectMigrationResultDTO where
  jsonSchema = genericJsonSchema jsonOptions
