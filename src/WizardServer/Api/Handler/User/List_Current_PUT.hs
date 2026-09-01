module WizardServer.Api.Handler.User.List_Current_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.Profile.UserProfileService
import WizardServer.Api.Resource.User.UserProfileChangeJM ()

type List_Current_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserProfileChangeDTO
    :> "users"
    :> "current"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_current_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserProfileChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] UserDTO)
list_current_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyUserProfile reqDto
