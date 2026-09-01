module WizardServer.Api.Handler.Tenant.Usage.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.Usage.Current_Wizard_GET
import WizardServer.Api.Handler.Tenant.Usage.Detail_Wizard_GET

type TenantUsageAPI =
  Tags "Tenant Usage"
    :> ( Current_Wizard_GET
           :<|> Detail_Wizard_GET
       )

tenantUsageApi :: Proxy TenantUsageAPI
tenantUsageApi = Proxy

tenantUsageServer :: WizardHandlerC s sm r rm => ServerT TenantUsageAPI sm
tenantUsageServer =
  current_wizard_GET
    :<|> detail_wizard_GET
