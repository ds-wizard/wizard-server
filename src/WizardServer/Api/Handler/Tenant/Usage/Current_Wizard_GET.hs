module WizardServer.Api.Handler.Tenant.Usage.Current_Wizard_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Tenant.Usage.WizardUsageService

type Current_Wizard_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "usages"
    :> "wizard"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

current_wizard_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
current_wizard_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUsageForCurrentTenant
