module WizardServer.Api.Handler.Tenant.PluginSettings.Detail_GET where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import WizardServer.Service.Tenant.PluginSettings.TenantPluginSettingsService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] A.Value)
detail_GET mTokenHeader mServerUrl pluginUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getPluginSettings pluginUuid
