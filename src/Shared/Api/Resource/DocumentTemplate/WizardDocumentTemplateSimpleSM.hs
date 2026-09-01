module Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateStateSM ()
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Util.Swagger

instance ToSchema DocumentTemplateSimpleDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimpleDTO
