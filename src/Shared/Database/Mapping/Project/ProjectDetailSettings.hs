module Shared.Database.Mapping.Project.ProjectDetailSettings where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Constant.DocumentTemplate
import Shared.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.Database.Mapping.Project.ProjectAcl
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectState ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Model.Project.Detail.ProjectDetailSettings

instance FromRow ProjectDetailSettings where
  fromRow = do
    uuid <- field
    name <- field
    description <- field
    visibility <- field
    sharing <- field
    isTemplate <- field
    projectTags <- fromPGArray <$> field
    selectedQuestionTagUuids <- fromPGArray <$> field
    language <- field
    formatUuid <- field
    documentTemplateLanguage <- field
    permissions <- loadPermissions uuid
    knowledgeModelPackageUuid <- field
    knowledgeModelPackageName <- field
    knowledgeModelPackageOrganizationId <- field
    knowledgeModelPackageKmId <- field
    knowledgeModelPackageVersion <- field
    knowledgeModelPackagePhase <- field
    knowledgeModelPackageDescription <- field
    knowledgeModelPackageNonEditable <- field
    knowledgeModelPackagePublic <- field
    knowledgeModelPackageLanguage <- field
    knowledgeModelPackageCreatedAt <- field
    let knowledgeModelPackage =
          KnowledgeModelPackageSimpleDTO
            { uuid = knowledgeModelPackageUuid
            , name = knowledgeModelPackageName
            , organizationId = knowledgeModelPackageOrganizationId
            , kmId = knowledgeModelPackageKmId
            , version = knowledgeModelPackageVersion
            , phase = knowledgeModelPackagePhase
            , remoteLatestVersion = Nothing
            , description = knowledgeModelPackageDescription
            , organization = Nothing
            , nonEditable = knowledgeModelPackageNonEditable
            , public = knowledgeModelPackagePublic
            , language = knowledgeModelPackageLanguage
            , createdAt = knowledgeModelPackageCreatedAt
            }
    let knowledgeModelTags = []
    let availableLocales = []
    mDocumentTemplateId <- field
    mDocumentTemplateName <- field
    mDocumentTemplateOrganizationId <- field
    mDocumentTemplateTemplateId <- field
    mDocumentTemplateVersion <- field
    mDocumentTemplatePhase <- field
    mDocumentTemplateDescription <- field
    mDocumentTemplateLanguage <- field
    mDocumentTemplateFormats <- fieldWith (optionalField fromJSONField)
    mDocumentTemplateLocales <- fieldWith (optionalField fromJSONField)
    let documentTemplate =
          case (mDocumentTemplateId, mDocumentTemplateName, mDocumentTemplateOrganizationId, mDocumentTemplateTemplateId, mDocumentTemplateVersion, mDocumentTemplateDescription, mDocumentTemplateLanguage, mDocumentTemplateFormats, mDocumentTemplateLocales) of
            (Just documentTemplateUuid, Just documentTemplateName, Just documentTemplateOrganizationId, Just documentTemplateTemplateId, Just documentTemplateVersion, Just documentTemplateDescription, Just documentTemplateLanguage, Just documentTemplateFormats, Just documentTemplateLocales) ->
              Just $
                DocumentTemplateSuggestionDTO
                  { uuid = documentTemplateUuid
                  , name = documentTemplateName
                  , organizationId = documentTemplateOrganizationId
                  , templateId = documentTemplateTemplateId
                  , version = documentTemplateVersion
                  , description = documentTemplateDescription
                  , language = documentTemplateLanguage
                  , formats = documentTemplateFormats
                  , locales = documentTemplateLocales
                  }
            _ -> Nothing
    let documentTemplatePhase = mDocumentTemplatePhase
    mDocumentTemplateMetamodelVersion <- field
    let documentTemplateSupportState =
          case mDocumentTemplateMetamodelVersion of
            Just metamodelVersion ->
              if isDocumentTemplateUnsupported metamodelVersion
                then Just UnsupportedMetamodelVersionDocumentTemplateState
                else Just DefaultDocumentTemplateState
            _ -> Nothing
    knowledgeModelState <- field
    documentTemplateState <- field
    fileCount <- field
    return $ ProjectDetailSettings {..}
