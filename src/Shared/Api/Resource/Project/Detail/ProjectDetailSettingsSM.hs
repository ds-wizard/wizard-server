module Shared.Api.Resource.Project.Detail.ProjectDetailSettingsSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Api.Resource.Project.Acl.ProjectPermSM ()
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadListSM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailSettingsJM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.Project.ProjectSharingSM ()
import Shared.Api.Resource.Project.ProjectStateSM ()
import Shared.Api.Resource.Project.ProjectVisibilitySM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Detail.ProjectDetailSettings
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectState
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import qualified Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper as PackageMapper
import Shared.Util.Swagger
import Shared.Util.Uuid

instance ToSchema ProjectDetailSettings where
  declareNamedSchema =
    toSwagger $
      ProjectDetailSettings
        { uuid = project1.uuid
        , name = project1.name
        , description = project1.description
        , visibility = project1.visibility
        , sharing = project1.sharing
        , selectedQuestionTagUuids = project1.selectedQuestionTagUuids
        , isTemplate = project1.isTemplate
        , permissions = [project1AlbertEditProjectPermDto]
        , projectTags = project1.projectTags
        , knowledgeModelPackageUuid = netherlandsKmPackageV2.uuid
        , knowledgeModelPackage = PackageMapper.toSimpleDTO netherlandsKmPackageV2
        , knowledgeModelTags = [tagDataScience]
        , knowledgeModelState = UpToDateKnowledgeModelProjectState
        , language = Nothing
        , availableLocales = []
        , documentTemplate = Just $ toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList]
        , documentTemplateState = Just UpToDateDocumentTemplateProjectState
        , documentTemplateSupportState = Just DefaultDocumentTemplateState
        , documentTemplatePhase = Just DraftDocumentTemplatePhase
        , formatUuid = Just . u' $ "ae3b9e68-e09e-4ad7-b476-67ab5626e873"
        , documentTemplateLanguage = Nothing
        , fileCount = 0
        }
