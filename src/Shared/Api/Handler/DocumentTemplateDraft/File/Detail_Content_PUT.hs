module Shared.Api.Handler.DocumentTemplateDraft.File.Detail_Content_PUT where

import qualified Data.Text as T
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.File.DocumentTemplateFileService

type Detail_Content_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[PlainText] T.Text
    :> "document-template-drafts"
    :> Capture "documentTemplateUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> "content"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

detail_content_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> T.Text
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
detail_content_PUT mTokenHeader mServerUrl reqContent _ fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyFileContent fileUuid (T.unpack reqContent)
