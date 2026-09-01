module WizardServer.Api.Handler.User.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserCreateDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService
import WizardServer.Api.Resource.User.UserCreateJM ()

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserCreateDTO
    :> "users"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

list_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> UserCreateDTO -> sm (Headers '[Header "x-trace-uuid" String] UserDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< registerOrCreateUserByAdmin reqDto
