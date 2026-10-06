module Shared.Api.Handler.UserGroup.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.Group.UserGroupListJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserGroupList
import Shared.Service.User.Group.UserGroupService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> QueryParam "q" String
    :> QueryParam "member" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserGroupList))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page UserGroupList))
list_GET mTokenHeader mServerUrl mW mTenant mQuery mMember mPage mSize mSort =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getUserGroupsPage mQuery mMember (Pageable mPage mSize) (parseSortQuery mSort)
