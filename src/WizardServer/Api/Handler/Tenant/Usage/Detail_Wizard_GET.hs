module WizardServer.Api.Handler.Tenant.Usage.Detail_Wizard_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Tenant.Usage.WizardUsageService

type Detail_Wizard_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> Capture "uuid" U.UUID
    :> "usages"
    :> "wizard"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

detail_wizard_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
detail_wizard_GET mTokenHeader mServerUrl tenantUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUsage tenantUuid
