module Shared.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.TransactionState
import Shared.Model.Document.Document
import Shared.Model.Error.Error
import Shared.Service.Document.DocumentService

type Detail_Documents_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "uuid" U.UUID
    :> "documents"
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_documents_preview_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_documents_preview_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $ do
      (doc, fileDto) <- createDocumentPreviewForDocTmlDraft uuid
      case doc.state of
        DoneDocumentState -> addTraceUuidHeader fileDto
        ErrorDocumentState ->
          throwError $ SystemLogError (_ERROR_SERVICE_PROJECT__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW doc.workerLog)
        _ -> throwError AcceptedError
