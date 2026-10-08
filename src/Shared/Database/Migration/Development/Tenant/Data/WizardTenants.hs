module Shared.Database.Migration.Development.Tenant.Data.WizardTenants (
  module Shared.Database.Migration.Development.Tenant.Data.Tenants,
  tenantCreateDto,
) where

import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Database.Migration.Development.Tenant.Data.Tenants

tenantCreateDto :: TenantCreateDTO
tenantCreateDto =
  TenantCreateDTO
    { tenantId = "new-tenant-id"
    , tenantName = "New Tenant"
    , firstName = "Max"
    , lastName = "Planck"
    , email = "max.planck@example.com"
    , password = "password"
    }
