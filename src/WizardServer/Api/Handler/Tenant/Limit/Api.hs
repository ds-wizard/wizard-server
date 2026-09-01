module WizardServer.Api.Handler.Tenant.Limit.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Tenant.Limit.List_PUT

type TenantLimitAPI =
  Tags "Tenant Limit"
    :> List_PUT

tenantLimitApi :: Proxy TenantLimitAPI
tenantLimitApi = Proxy

tenantLimitServer :: WizardHandlerC s sm r rm => ServerT TenantLimitAPI sm
tenantLimitServer = list_PUT
