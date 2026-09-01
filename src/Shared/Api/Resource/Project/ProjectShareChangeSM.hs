module Shared.Api.Resource.Project.ProjectShareChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.Acl.ProjectPermChangeSM ()
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Api.Resource.Project.ProjectShareChangeJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Util.Swagger

instance ToSchema ProjectShareChangeDTO where
  declareNamedSchema = toSwagger project1EditedShareChange
