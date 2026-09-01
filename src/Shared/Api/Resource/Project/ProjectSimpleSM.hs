module Shared.Api.Resource.Project.ProjectSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectSimpleJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.ProjectSimple
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectSimple where
  declareNamedSchema = toSwagger (toSimple project1)
