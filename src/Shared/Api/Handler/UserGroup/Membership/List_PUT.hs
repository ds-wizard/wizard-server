module Shared.Api.Handler.UserGroup.Membership.List_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.User.GroupMembership.UserGroupMembershipService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] [UserGroupMembershipChangeDTO]
    :> "user-groups"
    :> Capture "uuid" U.UUID
    :> "memberships"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserGroupMembershipChangeDTO])

list_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> [UserGroupMembershipChangeDTO]
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] [UserGroupMembershipChangeDTO])
list_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyUserGroupMembership uuid reqDto
