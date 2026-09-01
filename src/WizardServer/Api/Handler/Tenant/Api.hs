module WizardServer.Api.Handler.Tenant.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.Config.Api
import WizardServer.Api.Handler.Tenant.Detail_DELETE
import WizardServer.Api.Handler.Tenant.Detail_GET
import WizardServer.Api.Handler.Tenant.Detail_PUT
import WizardServer.Api.Handler.Tenant.Limit.Api
import WizardServer.Api.Handler.Tenant.List_GET
import WizardServer.Api.Handler.Tenant.List_POST
import WizardServer.Api.Handler.Tenant.List_Suggestions_GET
import WizardServer.Api.Handler.Tenant.PluginSettings.Api
import WizardServer.Api.Handler.Tenant.Usage.Api

type TenantAPI =
  Tags "Tenant"
    :> ( List_GET
           :<|> List_Suggestions_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> TenantConfigAPI
           :<|> TenantLimitAPI
           :<|> TenantPluginSettingsAPI
           :<|> TenantUsageAPI
       )

tenantApi :: Proxy TenantAPI
tenantApi = Proxy

tenantServer :: WizardHandlerC s sm r rm => ServerT TenantAPI sm
tenantServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> tenantConfigServer
    :<|> tenantLimitServer
    :<|> tenantPluginSettingsServer
    :<|> tenantUsageServer
