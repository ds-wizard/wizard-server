module WizardServer.Api.Handler.Tenant.Usage.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.Usage.Current_GET
import WizardServer.Api.Handler.Tenant.Usage.Detail_GET

type TenantUsageAPI =
  Tags "Tenant Usage"
    :> ( Current_GET
           :<|> Detail_GET
       )

tenantUsageApi :: Proxy TenantUsageAPI
tenantUsageApi = Proxy

tenantUsageServer :: WizardHandlerC s sm r rm => ServerT TenantUsageAPI sm
tenantUsageServer =
  current_GET
    :<|> detail_GET
