module WizardServer.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.Config.ClientConfigDTO
import WizardServer.Api.Resource.Config.ClientConfigJM ()
import WizardServer.Service.Config.Client.ClientConfigService

type List_Bootstrap_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "configs"
    :> "bootstrap"
    :> QueryParam "clientUrl" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET mTokenHeader mServerUrl mClientUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< getClientConfig mServerUrl mClientUrl
