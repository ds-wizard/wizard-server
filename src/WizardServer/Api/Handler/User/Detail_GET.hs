module WizardServer.Api.Handler.User.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserDetailDTO
import Shared.Api.Resource.User.UserDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.User.UserService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> Capture "uUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserDetailDTO)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] UserDetailDTO)
detail_GET mTokenHeader mServerUrl uUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUserDetailWithIdentitiesById uUuid
