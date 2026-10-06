module Shared.Api.Handler.PluginSettings.List_PUT where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Plugin.PluginChange
import Shared.Service.Plugin.PluginSettingsService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] (M.Map U.UUID PluginSettingsChangeDTO)
    :> "plugin-settings"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [PluginSettingsListDTO])

list_PUT
  :: WizardHandlerC s sm r rm
  => (PluginChange -> rm ())
  -> Maybe String
  -> Maybe String
  -> M.Map U.UUID PluginSettingsChangeDTO
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] [PluginSettingsListDTO])
list_PUT auditFn mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyPluginSettingsList auditFn reqDto
