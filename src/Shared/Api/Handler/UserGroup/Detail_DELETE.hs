module Shared.Api.Handler.UserGroup.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserGroupChange
import Shared.Service.User.Group.UserGroupService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> Capture "uuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: WizardHandlerC s sm r rm => (UserGroupChange -> rm ()) -> Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE auditFn mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteUserGroup auditFn uuid
        return NoContent
