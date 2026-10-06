module Shared.Api.Handler.DocumentTemplate.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.DocumentTemplate.DocumentTemplateService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> QueryParam "allVersions" Bool
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl uuid mAllVersions =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteDocumentTemplate uuid mAllVersions
        return NoContent
