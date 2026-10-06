module Shared.Api.Handler.UserGroup.List_Suggestions_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.Group.UserGroupSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserGroupSuggestion
import Shared.Service.User.Group.UserGroupService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> "suggestions"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mW mTenant mQuery mPage mSize mSort =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getUserGroupSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
