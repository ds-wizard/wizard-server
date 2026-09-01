module Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateFiles
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFileChangeDTO where
  declareNamedSchema = toSwagger fileDefaultHtmlEditedChangeDTO
