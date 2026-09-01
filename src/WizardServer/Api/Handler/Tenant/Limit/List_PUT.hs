module WizardServer.Api.Handler.Tenant.Limit.List_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Tenant.Limit.TenantLimitBundleChange
import Shared.Service.Tenant.Limit.WizardLimitService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantLimitBundleChange
    :> "tenants"
    :> Capture "uuid" U.UUID
    :> "limits"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

list_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> TenantLimitBundleChange
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
list_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyLimitBundle uuid reqDto
