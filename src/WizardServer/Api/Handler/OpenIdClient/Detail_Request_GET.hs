module WizardServer.Api.Handler.OpenIdClient.Detail_Request_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO
import Shared.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.OpenId.Client.Flow.OpenIdClientFlowService

type Detail_Request_GET =
  Header "Host" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "request"
    :> QueryParam "flow" String
    :> QueryParam "clientUrl" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientAuthenticationUrlDTO)

detail_request_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] OpenIdClientAuthenticationUrlDTO)
detail_request_GET mServerUrl providerUuid mFlow mClientUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< createAuthenticationUrl providerUuid mFlow mClientUrl
