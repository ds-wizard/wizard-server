module WizardServer.Api.Handler.Tenant.Config.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Config.WizardTenantConfigJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Tenant.Config.ConfigService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "config"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantConfig)

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] TenantConfig)
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getCurrentTenantConfigDto
