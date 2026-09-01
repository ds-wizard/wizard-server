module Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFormatSimple where
  declareNamedSchema = toSwagger formatJsonSimple
