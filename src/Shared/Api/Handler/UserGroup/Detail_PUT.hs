module Shared.Api.Handler.UserGroup.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Api.Resource.User.Group.UserGroupJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserGroup
import Shared.Service.User.Group.UserGroupService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserGroupChangeDTO
    :> "user-groups"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserGroup)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> UserGroupChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] UserGroup)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyUserGroup uuid reqDto
