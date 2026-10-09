module Shared.Api.Handler.Document.Preview where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Document.DocumentPreviewAcceptedDTO
import Shared.Api.Resource.Document.DocumentPreviewAcceptedJM ()
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Constant.Api (contentTypeHeaderJSON)
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Error.Error

type DocumentPreviewResponse = Headers '[Header "x-trace-uuid" String] TemporaryFileDTO

respondDocumentPreview
  :: WizardHandlerC s sm r rm
  => (rm (Either U.UUID DocumentPreviewResponse) -> sm (Either U.UUID DocumentPreviewResponse))
  -> rm (Document, TemporaryFileDTO)
  -> sm DocumentPreviewResponse
respondDocumentPreview runInService createPreview = do
  result <- runInService (createPreview >>= toDocumentPreviewResponse)
  either (throwError . documentPreviewAccepted) return result

toDocumentPreviewResponse :: WizardRequestContextC r rm => (Document, TemporaryFileDTO) -> rm (Either U.UUID DocumentPreviewResponse)
toDocumentPreviewResponse (doc, fileDto) =
  case doc.state of
    DoneDocumentState -> Right <$> addTraceUuidHeader fileDto
    ErrorDocumentState -> throwError $ SystemLogError (_ERROR_SERVICE_PROJECT__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW doc.workerLog)
    _ -> return $ Left doc.uuid

documentPreviewAccepted :: U.UUID -> ServerError
documentPreviewAccepted documentUuid =
  ServerError
    { errHTTPCode = 202
    , errReasonPhrase = "Accepted"
    , errBody = encode DocumentPreviewAcceptedDTO {documentUuid = documentUuid}
    , errHeaders = [contentTypeHeaderJSON]
    }
