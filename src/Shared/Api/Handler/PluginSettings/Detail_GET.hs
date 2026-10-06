module Shared.Api.Handler.PluginSettings.Detail_GET where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Plugin.PluginSettingsService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Description "tenant=true returns the organization values; w=<workspaceUuid> returns {overridden, overrideAllowed, values} with the effective values"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] A.Value)
detail_GET mTokenHeader mServerUrl pluginUuid mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getPluginSettings pluginUuid
