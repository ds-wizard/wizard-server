module WizardServer.Api.Handler.Locale.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Locale.LocaleSuggestion
import WizardServer.Service.Locale.LocaleService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page LocaleSuggestion))

list_suggestions_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] (Page LocaleSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getLocaleSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
