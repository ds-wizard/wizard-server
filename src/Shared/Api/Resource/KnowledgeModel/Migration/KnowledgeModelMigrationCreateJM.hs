module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelMigrationCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelMigrationCreateDTO where
  toJSON = genericToJSON jsonOptions
