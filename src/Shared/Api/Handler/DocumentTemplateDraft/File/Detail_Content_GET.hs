module Shared.Api.Handler.DocumentTemplateDraft.File.Detail_Content_GET where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService

type Detail_Content_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> "content"
    :> Get '[OctetStream] (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)

detail_content_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String, Header "Content-Type" String] FileStream)
detail_content_GET mTokenHeader mServerUrl _ fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      file <- getFile fileUuid
      let cdHeader = "text/plain;charset=utf-8"
      traceUuid <- asks (.traceUuid')
      return . addHeader (U.toString traceUuid) . addHeader cdHeader . FileStream . TE.encodeUtf8 . T.pack $ file.content
