module WizardServer.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Document.DocumentSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM ()
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Api.Resource.Locale.LocaleSuggestionSM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandListSM ()
import Shared.Api.Resource.PersistentCommand.WizardPersistentCommandSM ()
import Shared.Api.Resource.Project.Comment.ProjectCommentThreadAssignedSM ()
import Shared.Api.Resource.Project.Event.ProjectEventListSM ()
import Shared.Api.Resource.Project.File.ProjectFileListSM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Project.ProjectSM ()
import Shared.Api.Resource.Project.ProjectSuggestionSM ()
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.WizardTenantSM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserSM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Shared.Database.Migration.Development.Project.Data.ProjectComments
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Common.Page
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Locale.LocaleSuggestion
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Model.Project.Comment.ProjectCommentThreadAssigned
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectSuggestion
import Shared.Model.User.RoleList
import Shared.Model.User.UserSuggestion
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import qualified Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper as P_Mapper
import Shared.Service.Project.Event.ProjectEventMapper
import qualified Shared.Service.Project.ProjectMapper as PRJ_Mapper
import qualified Shared.Service.Tenant.TenantMapper as TNT_Mapper
import qualified Shared.Service.User.RoleMapper as R_Mapper
import qualified Shared.Service.User.WizardUserMapper as U_Mapper
import Shared.Util.Swagger
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleSM ()
import WizardServer.Api.Resource.User.Group.UserGroupSuggestionSM ()
import WizardServer.Api.Resource.User.RoleListSM ()
import WizardServer.Database.Migration.Development.Locale.Data.Locales
import WizardServer.Model.User.UserGroupSuggestion
import qualified WizardServer.Service.User.Group.UserGroupMapper as UG_Mapper

instance ToSchema (Page String) where
  declareNamedSchema = toSwaggerWithDtoName "Page String" (Page "projectTags" pageMetadata ["value1"])

instance ToSchema (Page UserDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page UserDTO" (Page "users" pageMetadata [U_Mapper.toDTO userAlbert])

instance ToSchema (Page RoleList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page RoleList" (Page "roles" pageMetadata [R_Mapper.toDTO adminRole 0])

instance ToSchema (Page UserSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page UserSuggestion"
      (Page "users" pageMetadata [U_Mapper.toSuggestion . U_Mapper.toSimple $ userAlbert])

instance ToSchema (Page UserGroupSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page UserGroupSuggestion" (Page "userGroups" pageMetadata [UG_Mapper.toSuggestion bioGroup])

instance ToSchema (Page LocaleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page Locale" (Page "locales" pageMetadata [localeNlDto])

instance ToSchema (Page LocaleSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page LocaleSuggestion" (Page "locales" pageMetadata [localeNlSuggestion])

instance ToSchema (Page KnowledgeModelPackageSimpleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page KnowledgeModelPackageSimpleDTO"
      (Page "knowledgeModelPackages" pageMetadata [P_Mapper.toSimpleDTO globalKmPackage])

instance ToSchema (Page KnowledgeModelPackageSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page KnowledgeModelPackageSuggestion"
      (Page "knowledgeModelPackages" pageMetadata [P_Mapper.toSuggestion globalKmPackage])

instance ToSchema (Page KnowledgeModelEditorList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page KnowledgeModelEditorList" (Page "knowledgeModelEditors" pageMetadata [amsterdamKnowledgeModelEditorList])

instance ToSchema (Page KnowledgeModelEditorSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page KnowledgeModelEditorSuggestion" (Page "knowledgeModelEditors" pageMetadata [amsterdamKnowledgeModelEditorSuggestion])

instance ToSchema (Page ProjectDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectDTO" (Page "projects" pageMetadata [project1Dto])

instance ToSchema (Page ProjectSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectSuggestion" (Page "projects" pageMetadata [PRJ_Mapper.toSuggestion project1])

instance ToSchema (Page ProjectCommentThreadAssigned) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectCommentThreadAssigned" (Page "commentThreads" pageMetadata [cmtAssigned])

instance ToSchema (Page ProjectEventList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectEventList"
      (Page "projectEvents" pageMetadata [SetReplyEventList' (toSetReplyEventList (sre_rQ1 project1.uuid) (Just userAlbert))])

instance ToSchema (Page ProjectFileList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectFileList"
      (Page "projectFiles" pageMetadata [projectFileList])

instance ToSchema (Page DocumentTemplateSimpleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page DocumentTemplateSimpleDTO" (Page "documentTemplates" pageMetadata [wizardDocumentTemplateSimpleDTO])

instance ToSchema (Page DocumentTemplateSuggestionDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page DocumentTemplateSuggestionDTO"
      (Page "documentTemplates" pageMetadata [toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList]])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = toSwaggerWithDtoName "Page DocumentDTO" (Page "documents" pageMetadata [doc1Dto])

instance ToSchema (Page PersistentCommandList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page PersistentCommandList"
      ( Page
          "persistentCommands"
          pageMetadata
          [command1List]
      )

instance ToSchema (Page TenantDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page TenantDTO" (Page "tenants" pageMetadata [TNT_Mapper.toDTO defaultTenant Nothing Nothing])

instance ToSchema (Page DocumentTemplateDraftList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page DocumentTemplateDraftList" (Page "documentTemplateDrafts" pageMetadata [toDraftList wizardDocumentTemplate])
