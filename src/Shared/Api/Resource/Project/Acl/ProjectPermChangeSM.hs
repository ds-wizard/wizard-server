module Shared.Api.Resource.Project.Acl.ProjectPermChangeSM where

import Data.Swagger

import Shared.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Shared.Api.Resource.Project.Acl.ProjectPermChangeJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectPermChangeDTO where
  declareNamedSchema = toSwagger (toProjectPermChangeDTO project1AlbertEditProjectPerm)
