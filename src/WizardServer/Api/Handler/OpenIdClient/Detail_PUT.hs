module WizardServer.Api.Handler.OpenIdClient.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OpenIdClientChangeDTO
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> OpenIdClientChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyOpenIdClientDefinition uuid reqDto
