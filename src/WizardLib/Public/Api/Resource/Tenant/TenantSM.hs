module WizardLib.Public.Api.Resource.Tenant.TenantSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.TenantJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants
import WizardLib.Public.Model.Tenant.Tenant

instance ToSchema Tenant where
  declareNamedSchema = toSwagger defaultTenant

instance ToSchema TenantState where
  declareNamedSchema = toSwagger ReadyForUseTenantState

instance ToParamSchema TenantState
