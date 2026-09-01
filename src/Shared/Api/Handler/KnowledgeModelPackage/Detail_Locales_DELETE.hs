module Shared.Api.Handler.KnowledgeModelPackage.Detail_Locales_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Capture "localeUuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_locales_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_locales_DELETE mTokenHeader mServerUrl pkgUuid localeUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteLocale pkgUuid localeUuid
        return NoContent
