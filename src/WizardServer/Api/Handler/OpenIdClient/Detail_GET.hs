module WizardServer.Api.Handler.OpenIdClient.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getOpenIdClientDefinitionByUuid uuid
