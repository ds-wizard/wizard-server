module WizardServer.Api.Handler.Tenant.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.PluginSettings.Detail_GET
import WizardServer.Api.Handler.Tenant.PluginSettings.Detail_PUT
import WizardServer.Api.Handler.Tenant.PluginSettings.List_PUT

type TenantPluginSettingsAPI =
  Tags "Tenant Plugin Settings"
    :> ( List_PUT
           :<|> Detail_GET
           :<|> Detail_PUT
       )

tenantPluginSettingsApi :: Proxy TenantPluginSettingsAPI
tenantPluginSettingsApi = Proxy

tenantPluginSettingsServer :: WizardHandlerC s sm r rm => ServerT TenantPluginSettingsAPI sm
tenantPluginSettingsServer =
  list_PUT
    :<|> detail_GET
    :<|> detail_PUT
