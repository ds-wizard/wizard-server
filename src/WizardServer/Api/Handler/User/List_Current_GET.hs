module WizardServer.Api.Handler.User.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon hiding (getCurrentUser)
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.User.Profile.UserProfileService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_current_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] UserDTO)
list_current_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getUserProfile
