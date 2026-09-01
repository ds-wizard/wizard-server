module Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Util.Swagger

instance ToSchema DocumentTemplateLocaleList where
  declareNamedSchema = toSwagger czechWizardDocumentTemplateLocaleList
