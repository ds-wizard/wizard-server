module Shared.Api.Handler.PluginSettings.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Model.Plugin.PluginChange
import Shared.Service.Plugin.PluginSettingsService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "plugin-settings"
    :> Capture "pluginUuid" U.UUID
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE
  :: WizardHandlerC s sm r rm
  => (PluginChange -> rm ())
  -> Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE auditFn mTokenHeader mServerUrl pluginUuid mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deletePluginSettings auditFn pluginUuid
        return NoContent
