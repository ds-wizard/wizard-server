module Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates where

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Database.Migration.Development.Registry.Data.RegistryTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper

wizardDocumentTemplateSimpleDTO :: DocumentTemplateSimpleDTO
wizardDocumentTemplateSimpleDTO =
  toSimpleDTO'
    True
    ( toList
        wizardDocumentTemplate
        (Just commonWizardRegistryTemplate)
        (Just globalRegistryOrganization)
        ReleasedDocumentTemplatePhase
    )

wizardDocumentTemplateDetailDTO :: DocumentTemplateDetailDTO
wizardDocumentTemplateDetailDTO =
  toDetailDTO
    wizardDocumentTemplate
    wizardDocumentTemplateFormats
    True
    [commonWizardRegistryTemplate]
    [globalRegistryOrganization]
    [(wizardDocumentTemplate.uuid, wizardDocumentTemplate.version)]
    (Just "https://registry-test.ds-wizard.org/document-templates/global:project-report:1.0.0")
    [globalKmPackage, netherlandsKmPackageV2]
    [czechWizardDocumentTemplateLocaleList]

wizardDocumentTemplateDeprecatedChangeDTO :: DocumentTemplateChangeDTO
wizardDocumentTemplateDeprecatedChangeDTO = toChangeDTO wizardDocumentTemplateDeprecated

wizardDocumentTemplateWithCoordinate :: DocumentTemplateWithCoordinate
wizardDocumentTemplateWithCoordinate = toWithCoordinate wizardDocumentTemplate
