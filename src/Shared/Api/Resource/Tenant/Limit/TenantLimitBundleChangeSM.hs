module Shared.Api.Resource.Tenant.Limit.TenantLimitBundleChangeSM where

import Data.Swagger

import Shared.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Shared.Model.Tenant.Limit.TenantLimitBundleChange
import Shared.Util.Swagger

instance ToSchema TenantLimitBundleChange where
  declareNamedSchema = toSwagger tenantLimitBundleChange
