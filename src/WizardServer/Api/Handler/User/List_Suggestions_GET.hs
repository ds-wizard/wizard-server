module WizardServer.Api.Handler.User.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserSuggestion
import Shared.Service.User.UserService
import Shared.Util.String (splitOn)

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "select" String
    :> QueryParam "exclude" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))

list_suggestions_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelect mExclude mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mSelectUuids = fmap (splitOn ",") mSelect
        let mExcludeUuids = fmap (splitOn ",") mExclude
        getUserSuggestionsPage mQuery mSelectUuids mExcludeUuids (Pageable mPage mSize) (parseSortQuery mSort)
