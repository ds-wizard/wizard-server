module Shared.Api.Resource.Tenant.TenantSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantJM ()
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Tenant.Tenant
import Shared.Util.Swagger

instance ToSchema Tenant where
  declareNamedSchema = toSwagger defaultTenant

instance ToSchema TenantState where
  declareNamedSchema = toSwagger ReadyForUseTenantState

instance ToParamSchema TenantState
