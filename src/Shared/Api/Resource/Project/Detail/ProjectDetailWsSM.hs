module Shared.Api.Resource.Project.Detail.ProjectDetailWsSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailWsJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectDetailWsDTO where
  declareNamedSchema = toSwagger (toDetailWsDTO project1 Nothing Nothing [] M.empty M.empty M.empty)
