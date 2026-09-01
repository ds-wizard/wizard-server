module Shared.Api.Resource.Project.Detail.ProjectDetailPreviewSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailPreviewJM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Project.Detail.ProjectDetailPreview
import Shared.Model.Project.Project
import qualified Shared.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import Shared.Util.Swagger

instance ToSchema ProjectDetailPreview where
  declareNamedSchema =
    toSwagger $
      ProjectDetailPreview
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackage = germanyPackageSuggestion
        , isTemplate = project1.isTemplate
        , documentTemplateUuid = Just wizardDocumentTemplate.uuid
        , permissions = [project1AlbertEditProjectPermDto]
        , format = Just . DocumentTemplateMapper.toFormatSimple $ formatJson
        , fileCount = 0
        }
