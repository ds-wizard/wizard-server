module Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateFiles where

import Shared.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Model.DocumentTemplate.DocumentTemplate

fileDefaultHtmlEditedChangeDTO :: DocumentTemplateFileChangeDTO
fileDefaultHtmlEditedChangeDTO =
  DocumentTemplateFileChangeDTO
    { fileName = fileDefaultHtmlEdited.fileName
    , content = fileDefaultHtmlEdited.content
    }

fileNewFileChangeDTO :: DocumentTemplateFileChangeDTO
fileNewFileChangeDTO =
  DocumentTemplateFileChangeDTO
    { fileName = fileNewFile.fileName
    , content = fileNewFile.content
    }
