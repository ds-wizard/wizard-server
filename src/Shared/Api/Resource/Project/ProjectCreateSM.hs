module Shared.Api.Resource.Project.ProjectCreateSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectCreateDTO where
  declareNamedSchema = toSwagger project1Create
