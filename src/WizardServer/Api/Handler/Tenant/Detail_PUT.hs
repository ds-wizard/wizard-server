module WizardServer.Api.Handler.Tenant.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Tenant
import Shared.Service.Tenant.TenantService
import WizardServer.Api.Resource.Tenant.TenantChangeJM ()

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantChangeDTO
    :> "tenants"
    :> Capture "aUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Tenant)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> TenantChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] Tenant)
detail_PUT mTokenHeader mServerUrl reqDto aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyTenant aUuid reqDto
