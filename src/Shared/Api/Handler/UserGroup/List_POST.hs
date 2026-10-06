module Shared.Api.Handler.UserGroup.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Api.Resource.User.Group.UserGroupJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupChange
import Shared.Service.User.Group.UserGroupService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserGroupChangeDTO
    :> "user-groups"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserGroup)

list_POST
  :: WizardHandlerC s sm r rm
  => (UserGroupChange -> rm ())
  -> Maybe String
  -> Maybe String
  -> UserGroupChangeDTO
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] UserGroup)
list_POST auditFn mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createUserGroup auditFn reqDto
