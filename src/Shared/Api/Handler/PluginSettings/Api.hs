module Shared.Api.Handler.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.PluginSettings.Detail_DELETE
import Shared.Api.Handler.PluginSettings.Detail_GET
import Shared.Api.Handler.PluginSettings.Detail_PUT
import Shared.Api.Handler.PluginSettings.List_GET
import Shared.Api.Handler.PluginSettings.List_PUT
import Shared.Api.Handler.WizardCommon
import Shared.Model.Plugin.PluginChange

type PluginSettingsAPI =
  Tags "Plugin Settings"
    :> ( List_GET
           :<|> List_PUT
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
       )

pluginSettingsApi :: Proxy PluginSettingsAPI
pluginSettingsApi = Proxy

pluginSettingsServer :: WizardHandlerC s sm r rm => (PluginChange -> rm ()) -> ServerT PluginSettingsAPI sm
pluginSettingsServer auditFn =
  list_GET
    :<|> list_PUT auditFn
    :<|> detail_GET
    :<|> detail_PUT auditFn
    :<|> detail_DELETE auditFn
