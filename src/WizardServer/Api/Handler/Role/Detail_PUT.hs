module WizardServer.Api.Handler.Role.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.RoleChangeJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.RoleList
import WizardServer.Service.User.Role.RoleService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] RoleChangeDTO
    :> "roles"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] RoleList)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> RoleChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] RoleList)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyRole uuid reqDto
