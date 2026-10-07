module Shared.Api.Handler.Locale.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Locale.LocaleService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [LocaleDTO])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getLocales
