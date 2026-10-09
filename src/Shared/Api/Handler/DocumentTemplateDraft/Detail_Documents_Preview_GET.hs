module Shared.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Document.Preview
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Document.DocumentService

type Detail_Documents_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "uuid" U.UUID
    :> "documents"
    :> "preview"
    :> QueryParam "documentUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_documents_preview_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe U.UUID
  -> sm DocumentPreviewResponse
detail_documents_preview_GET mTokenHeader mServerUrl uuid mDocumentUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    respondDocumentPreview (runInMaybeAuthService Transactional) (createDocumentPreviewForDocTmlDraft uuid mDocumentUuid)
