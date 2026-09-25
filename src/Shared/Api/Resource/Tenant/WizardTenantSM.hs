module Shared.Api.Resource.Tenant.WizardTenantSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.TenantSM ()
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Service.Tenant.TenantMapper
import Shared.Util.Swagger

instance ToSchema TenantDTO where
  declareNamedSchema = toSwagger (toDTO defaultTenant Nothing Nothing)
