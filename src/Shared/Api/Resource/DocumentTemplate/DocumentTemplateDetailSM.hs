module Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Api.Resource.Registry.RegistryOrganizationSM ()
import Shared.Api.Resource.Version.VersionSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDetailDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDetailDTO
