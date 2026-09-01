module WizardServer.Api.Handler.User.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService
import WizardServer.Api.Resource.User.UserChangeJM ()

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserChangeDTO
    :> "users"
    :> Capture "uUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] UserDTO)
detail_PUT mTokenHeader mServerUrl reqDto uUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyUser uUuid reqDto
