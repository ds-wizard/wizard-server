module Shared.Api.Resource.Project.ProjectCreateFromTemplateSM where

import Data.Swagger

import Shared.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Shared.Api.Resource.Project.ProjectCreateFromTemplateJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectCreateFromTemplateDTO where
  declareNamedSchema = toSwagger (toCreateFromTemplateDTO project1)
