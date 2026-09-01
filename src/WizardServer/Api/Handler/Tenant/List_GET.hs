module WizardServer.Api.Handler.Tenant.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Tenant
import Shared.Service.Tenant.TenantService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> QueryParam "q" String
    :> QueryParam "states" [TenantState]
    :> QueryParam "enabled" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page TenantDTO))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe [TenantState]
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page TenantDTO))
list_GET mTokenHeader mServerUrl mQuery mStates mEnabled mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTenantsPage mQuery mStates mEnabled (Pageable mPage mSize) (parseSortQuery mSort)
