module Shared.Api.Handler.DocumentTemplate.Detail_Locales_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleService

type Detail_Locales_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateLocaleList])

detail_locales_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [DocumentTemplateLocaleList])
detail_locales_GET mTokenHeader mServerUrl dtUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getLocalesForDocumentTemplate dtUuid
