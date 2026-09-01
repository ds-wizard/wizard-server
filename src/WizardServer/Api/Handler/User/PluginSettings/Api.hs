module WizardServer.Api.Handler.User.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.User.PluginSettings.Detail_GET
import WizardServer.Api.Handler.User.PluginSettings.Detail_PUT

type PluginSettingsAPI =
  Tags "User Plugin Settings"
    :> ( Detail_GET
           :<|> Detail_PUT
       )

pluginSettingsApi :: Proxy PluginSettingsAPI
pluginSettingsApi = Proxy

pluginSettingsServer :: WizardHandlerC s sm r rm => ServerT PluginSettingsAPI sm
pluginSettingsServer =
  detail_GET
    :<|> detail_PUT
