module Shared.Api.Handler.Project.Detail_DocumentContext_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.Document.DocumentContext
import Shared.Model.Document.DocumentContextJM ()
import Shared.Service.Document.Context.DocumentContextService

type Detail_DocumentContext_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "document-context"
    :> QueryParam "eventUuid" U.UUID
    :> Description "The body's hash is returned as a strong ETag (\"<hash>\") with Cache-Control: private, no-cache; ETag and x-trace-uuid are exposed through CORS. eventUuid returns the questionnaire content as of that event."
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String, Header "ETag" String, Header "Cache-Control" String] DocumentContext)

detail_documentContext_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String, Header "ETag" String, Header "Cache-Control" String] DocumentContext)
detail_documentContext_GET mTokenHeader mServerUrl uuid mEventUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ do
      ctx <- getProjectDocumentContext uuid mEventUuid
      addEtagHeaders ctx.hash ctx
