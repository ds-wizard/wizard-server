module Shared.Api.Resource.Tenant.Usage.WizardUsageSM where

import Data.Swagger

import Shared.Api.Resource.Tenant.Usage.UsageEntrySM ()
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Database.Migration.Development.Tenant.Data.TenantUsages
import Shared.Util.Swagger

instance ToSchema WizardUsageDTO where
  declareNamedSchema = toSwagger defaultUsage
