module WizardServer.Api.Handler.User.Detail_State_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserStateDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService
import WizardServer.Api.Resource.User.UserStateJM ()

type Detail_State_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserStateDTO
    :> "users"
    :> Capture "uUuid" String
    :> "state"
    :> QueryParam' '[Required] "hash" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserStateDTO)

detail_state_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserStateDTO
  -> String
  -> String
  -> sm (Headers '[Header "x-trace-uuid" String] UserStateDTO)
detail_state_PUT mTokenHeader mServerUrl reqDto uUuid hash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        changeUserState hash reqDto.active
        return reqDto
