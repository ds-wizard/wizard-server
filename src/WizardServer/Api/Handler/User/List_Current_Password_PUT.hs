module WizardServer.Api.Handler.User.List_Current_Password_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import Shared.Service.User.Profile.UserProfileService
import WizardServer.Api.Resource.User.UserPasswordJM ()

type List_Current_Password_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> "current"
    :> "password"
    :> Verb PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_password_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserPasswordDTO
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_password_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        user <- getCurrentUser
        changeUserProfilePassword user.uuid reqDto
        return NoContent
