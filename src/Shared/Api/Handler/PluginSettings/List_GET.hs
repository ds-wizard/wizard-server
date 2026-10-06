module Shared.Api.Handler.PluginSettings.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Plugin.PluginSettingsDTO
import Shared.Api.Resource.Plugin.PluginSettingsJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Plugin.PluginSettingsService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "plugin-settings"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [PluginSettingsListDTO])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] [PluginSettingsListDTO])
list_GET mTokenHeader mServerUrl mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getPluginSettingsList
