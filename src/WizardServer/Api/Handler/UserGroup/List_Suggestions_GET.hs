module WizardServer.Api.Handler.UserGroup.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import WizardServer.Api.Resource.User.Group.UserGroupSuggestionJM ()
import WizardServer.Model.User.UserGroupSuggestion
import WizardServer.Service.User.Group.UserGroupService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getUserGroupSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
