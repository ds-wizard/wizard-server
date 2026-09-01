module Shared.Api.Resource.Project.Detail.ProjectDetailSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Project.Project
import Shared.Util.Swagger

instance ToSchema ProjectDetailDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackage = germanyPackageSuggestion
        , isTemplate = project1.isTemplate
        , permissions = [project1AlbertEditProjectPermDto]
        , fileCount = 0
        }
