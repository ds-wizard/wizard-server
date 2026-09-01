module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM ()
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateSuggestionDTO where
  declareNamedSchema = toSwagger (toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList])
