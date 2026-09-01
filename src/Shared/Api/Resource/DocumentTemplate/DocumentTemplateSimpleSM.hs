module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Util.Swagger

instance ToSchema DocumentTemplateSimple where
  declareNamedSchema = toSwagger wizardDocumentTemplateSimple
