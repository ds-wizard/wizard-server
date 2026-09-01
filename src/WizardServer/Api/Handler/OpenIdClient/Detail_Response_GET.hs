module WizardServer.Api.Handler.OpenIdClient.Detail_Response_GET where

import Data.Maybe (isJust)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Model.Context.TransactionState
import WizardServer.Service.OpenId.Client.Flow.OpenIdClientFlowService

type Detail_Response_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "response"
    :> QueryParam "clientUrl" String
    :> QueryParam "error" String
    :> QueryParam "code" String
    :> QueryParam "state" String
    :> QueryParam "id_token" String
    :> QueryParam "session_state" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

detail_response_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
detail_response_GET mTokenHeader mServerUrl mUserAgent providerUuid mClientUrl mError mCode mState mIdToken mSessionState =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< loginUserOrLinkIdentity (isJust mTokenHeader) providerUuid mClientUrl mError mCode mState mIdToken mUserAgent mSessionState
