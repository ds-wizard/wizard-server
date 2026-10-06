module WizardServer.Api.Handler.Locale.List_DELETE where

import Data.Maybe (maybeToList)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Service.Locale.LocaleService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> QueryParam "id" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl mId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = maybeToList ((,) "id" <$> mId)
        deleteLocalesByQueryParams queryParams
        return NoContent
