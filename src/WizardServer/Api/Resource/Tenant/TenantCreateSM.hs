module WizardServer.Api.Resource.Tenant.TenantCreateSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Util.Swagger
import WizardServer.Api.Resource.Tenant.TenantCreateJM ()

instance ToSchema TenantCreateDTO where
  declareNamedSchema = toSwagger tenantCreateDto
