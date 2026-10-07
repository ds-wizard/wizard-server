module Shared.Api.Handler.Locale.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleSuggestionDTO
import Shared.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Locale.LocaleService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "suggestions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [LocaleSuggestionDTO])

list_suggestions_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [LocaleSuggestionDTO])
list_suggestions_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getLocaleSuggestions
