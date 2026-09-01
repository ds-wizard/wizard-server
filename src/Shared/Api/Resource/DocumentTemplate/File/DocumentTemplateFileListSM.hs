module Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileListJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateFileList where
  declareNamedSchema = toSwagger (toList fileDefaultHtml)
