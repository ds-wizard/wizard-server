module Shared.Database.Mapping.Document.DocumentList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Database.Mapping.Document.Document ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Model.Document.DocumentList

instance FromRow DocumentList where
  fromRow = do
    uuid <- field
    name <- field
    state <- field
    projectUuid <- field
    projectName <- field
    projectEventUuid <- field
    projectVersion <- field
    documentTemplate <- fieldDocumentTemplateWithCoordinate
    documentTemplateFormat <- fieldDocumentTemplateFormatSimple
    language <- field
    fileSize <- field
    workerLog <- field
    createdBy <- field
    createdAt <- field
    return $ DocumentList {..}
