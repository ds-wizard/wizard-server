module WizardServer.Api.Handler.OpenIdClient.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.OpenId.OpenIdClientSimple
import WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "open-id-clients"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OpenIdClientSimple])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [OpenIdClientSimple])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getOpenIdClientDefinitions
