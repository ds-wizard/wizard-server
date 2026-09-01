module WizardServer.Api.Resource.Tenant.TenantChangeSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Service.Tenant.TenantMapper
import Shared.Util.Swagger
import WizardServer.Api.Resource.Tenant.TenantChangeJM ()

instance ToSchema TenantChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO defaultTenant)
