module Shared.Api.Resource.Tenant.TenantSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.TenantSuggestionJM ()
import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Model.Tenant.TenantSuggestion
import Shared.Util.Swagger

instance ToSchema TenantSuggestion where
  declareNamedSchema = toSwagger tenantSuggestion
