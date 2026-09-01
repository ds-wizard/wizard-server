module Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Util.Swagger

instance ToSchema DocumentTemplateChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDeprecatedChangeDTO
