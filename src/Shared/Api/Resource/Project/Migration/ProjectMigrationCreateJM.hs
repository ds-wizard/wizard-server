module Shared.Api.Resource.Project.Migration.ProjectMigrationCreateJM where

import Data.Aeson

import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Util.Aeson

instance FromJSON ProjectMigrationCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectMigrationCreateDTO where
  toJSON = genericToJSON jsonOptions
