module Shared.Api.Resource.Project.Migration.ProjectMigrationCreateSM where

import Data.Swagger

import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectMigrationCreateDTO where
  declareNamedSchema = toSwagger projectMigrationCreateDto
