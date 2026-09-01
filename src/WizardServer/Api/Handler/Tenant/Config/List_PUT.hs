module WizardServer.Api.Handler.Tenant.Config.List_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Tenant.Config.ConfigService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantConfigChangeDTO
    :> "tenants"
    :> "current"
    :> "config"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantConfig)

list_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> TenantConfigChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] TenantConfig)
list_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyTenantConfigDto reqDto
