module Shared.Api.Handler.PluginSettings.Detail_PUT where

import qualified Data.Aeson as A
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.Plugin.PluginChange
import Shared.Service.Plugin.PluginSettingsService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] A.Value
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Description "tenant=true stores and returns the organization values; w=<workspaceUuid> creates or replaces the workspace override and returns {overridden, overrideAllowed, values}"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] A.Value)

detail_PUT
  :: WizardHandlerC s sm r rm
  => (PluginChange -> rm ())
  -> Maybe String
  -> Maybe String
  -> A.Value
  -> U.UUID
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] A.Value)
detail_PUT auditFn mTokenHeader mServerUrl reqDto pluginUuid mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyPluginSettings auditFn pluginUuid reqDto
