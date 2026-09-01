module WizardServer.Api.Handler.Tenant.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Tenant.TenantService
import WizardServer.Api.Resource.Tenant.TenantCreateJM ()

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantCreateDTO
    :> "tenants"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantDTO)

list_POST
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> TenantCreateDTO -> sm (Headers '[Header "x-trace-uuid" String] TenantDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< registerOrCreateTenantByAdmin reqDto
