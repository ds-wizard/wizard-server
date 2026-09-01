module WizardServer.Api.Handler.UserGroup.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Model.Context.TransactionState
import WizardServer.Service.User.Group.UserGroupService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserGroupDetailDTO)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] UserGroupDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUserGroupByUuid uuid
