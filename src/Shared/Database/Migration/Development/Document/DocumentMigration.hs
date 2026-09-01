module Shared.Database.Migration.Development.Document.DocumentMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.S3.Document.DocumentS3
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Document/Document) started"
  deleteDocuments
  removeDocumentContents
  insertDocument doc1
  insertDocument doc2
  insertDocument doc3
  insertDocument differentDoc
  putDocumentContent doc1.uuid doc1Content
  logInfo _CMP_MIGRATION "(Document/Document) ended"
