module WizardServer.Api.Handler.Bootstrap.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.Bootstrap.BootstrapDTO
import WizardServer.Api.Resource.Bootstrap.BootstrapJM ()
import WizardServer.Service.Bootstrap.BootstrapService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "bootstrap"
    :> QueryParam "clientUrl" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] BootstrapDTO)

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] BootstrapDTO)
list_GET mTokenHeader mServerUrl mClientUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< getBootstrap mServerUrl mClientUrl
