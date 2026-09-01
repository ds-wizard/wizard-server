module Shared.Api.Resource.Project.Version.ProjectVersionChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Api.Resource.Project.Version.ProjectVersionChangeJM ()
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectVersionChangeDTO where
  declareNamedSchema = toSwagger (projectVersion2ChangeDto project1Uuid)
