module Shared.Api.Resource.Project.ProjectSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectJM ()
import Shared.Api.Resource.Project.ProjectReportSM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectStateSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.User.UserSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.ProjectState
import Shared.Service.Project.ProjectMapper
import Shared.Util.Swagger

instance ToSchema ProjectDTO where
  declareNamedSchema =
    toSwagger (toDTO project1 germanyKmPackage UpToDateKnowledgeModelProjectState (Just UpToDateDocumentTemplateProjectState) [project1AlbertEditProjectPermDto])
