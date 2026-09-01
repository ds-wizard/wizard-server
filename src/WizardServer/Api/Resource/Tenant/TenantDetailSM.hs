module WizardServer.Api.Resource.Tenant.TenantDetailSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageSM ()
import Shared.Api.Resource.Tenant.WizardTenantSM ()
import Shared.Api.Resource.User.UserSM ()
import Shared.Database.Migration.Development.Tenant.Data.TenantUsages
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Service.Tenant.TenantMapper
import Shared.Util.Swagger
import WizardServer.Api.Resource.Tenant.TenantDetailJM ()

instance ToSchema TenantDetailDTO where
  declareNamedSchema =
    toSwagger (toDetailDTO defaultTenant Nothing Nothing defaultUsage [])
