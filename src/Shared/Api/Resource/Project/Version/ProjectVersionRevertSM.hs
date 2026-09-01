module Shared.Api.Resource.Project.Version.ProjectVersionRevertSM where

import Data.Swagger

import Shared.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Shared.Api.Resource.Project.Version.ProjectVersionRevertJM ()
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectVersionRevertDTO where
  declareNamedSchema = toSwagger (projectVersion1RevertDto project1Uuid)
