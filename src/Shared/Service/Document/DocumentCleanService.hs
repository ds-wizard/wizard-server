module Shared.Service.Document.DocumentCleanService where

import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.S3.Document.DocumentS3

cleanDocuments :: WizardRequestContextC s m => m ()
cleanDocuments =
  runInTransaction $ do
    docs <- findDocumentsFiltered [("durability", "TemporallyDocumentDurability")]
    let docsFiltered = filter (\d -> d.state == DoneDocumentState || d.state == ErrorDocumentState) docs
    traverse_
      ( \d -> do
          deleteDocumentByUuidAndTenantUuid d.uuid d.tenantUuid
          removeDocumentContentWithTenant d.tenantUuid d.uuid
      )
      docsFiltered

cleanTemporallyDocumentsForTemplate :: WizardRequestContextC s m => U.UUID -> m ()
cleanTemporallyDocumentsForTemplate dtUuid =
  void $ deleteDocumentsFiltered [("document_template_uuid", U.toString dtUuid), ("durability", "TemporallyDocumentDurability")]
