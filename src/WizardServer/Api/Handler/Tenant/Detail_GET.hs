module WizardServer.Api.Handler.Tenant.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Model.Context.TransactionState
import Shared.Service.Tenant.TenantService
import WizardServer.Api.Resource.Tenant.TenantDetailJM ()

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> Capture "aUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantDetailDTO)

detail_GET
  :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] TenantDetailDTO)
detail_GET mTokenHeader mServerUrl aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getTenantByUuid aUuid
