module WizardServer.Api.Handler.User.Detail_Password_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService
import WizardServer.Api.Resource.User.UserPasswordJM ()

type Detail_Password_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserPasswordDTO
    :> "users"
    :> Capture "uuid" U.UUID
    :> "password"
    :> QueryParam "hash" String
    :> Verb PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_password_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserPasswordDTO
  -> U.UUID
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_password_PUT mTokenHeader mServerUrl reqDto uuid mHash =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        changeUserPasswordByAdminOrHash uuid reqDto mHash
        return NoContent
