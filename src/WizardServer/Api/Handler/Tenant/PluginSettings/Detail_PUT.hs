module WizardServer.Api.Handler.Tenant.PluginSettings.Detail_PUT where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Service.Tenant.PluginSettings.TenantPluginSettingsService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] A.Value
    :> "tenants"
    :> "current"
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> A.Value -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] A.Value)
detail_PUT mTokenHeader mServerUrl reqDto pluginUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< createOrUpdatePluginSettings pluginUuid reqDto
