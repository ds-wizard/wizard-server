module WizardServer.Api.Handler.Tenant.Config.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.Config.List_GET
import WizardServer.Api.Handler.Tenant.Config.List_PUT

type TenantConfigAPI =
  Tags "Tenant Config"
    :> ( List_GET
           :<|> List_PUT
       )

tenantConfigApi :: Proxy TenantConfigAPI
tenantConfigApi = Proxy

tenantConfigServer :: WizardHandlerC s sm r rm => ServerT TenantConfigAPI sm
tenantConfigServer = list_GET :<|> list_PUT
