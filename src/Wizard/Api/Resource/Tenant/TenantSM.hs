module Wizard.Api.Resource.Tenant.TenantSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Service.Tenant.TenantMapper
import WizardLib.Public.Api.Resource.Tenant.TenantSM ()

instance ToSchema TenantDTO where
  declareNamedSchema = toSwagger (toDTO defaultTenant Nothing Nothing)
