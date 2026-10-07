module WizardServer.Api.Handler.Locale.Registry.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Api.Resource.Locale.Registry.RegistryLocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Locale.Registry.RegistryLocaleService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "registry"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [RegistryLocaleDTO])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [RegistryLocaleDTO])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getRegistryLocales
