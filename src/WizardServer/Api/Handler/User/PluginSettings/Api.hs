module WizardServer.Api.Handler.User.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.User.PluginSettings.Detail_GET
import WizardServer.Api.Handler.User.PluginSettings.Detail_PUT

type UserPluginSettingsAPI =
  Tags "User Plugin Settings"
    :> ( Detail_GET
           :<|> Detail_PUT
       )

userPluginSettingsApi :: Proxy UserPluginSettingsAPI
userPluginSettingsApi = Proxy

userPluginSettingsServer :: WizardHandlerC s sm r rm => ServerT UserPluginSettingsAPI sm
userPluginSettingsServer =
  detail_GET
    :<|> detail_PUT
