module WizardServer.Api.Handler.Tenant.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.TenantSuggestionJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.TenantSuggestion
import Shared.Service.Tenant.TenantService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "suggestions"
    :> QueryParam "q" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TenantSuggestion])

list_suggestions_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [TenantSuggestion])
list_suggestions_GET mTokenHeader mServerUrl mQuery =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTenantSuggestions mQuery
