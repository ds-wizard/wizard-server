module WizardServer.Api.Handler.OpenIdClient.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OpenIdClientChangeDTO
    :> "open-id-clients"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> OpenIdClientChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createOpenIdClientDefinition reqDto
