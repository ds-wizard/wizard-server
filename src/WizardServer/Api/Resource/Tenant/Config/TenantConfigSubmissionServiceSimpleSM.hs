module WizardServer.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleSM where

import Data.Swagger

import Shared.Api.Resource.Config.SimpleFeatureSM ()
import Shared.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM ()
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Service.Tenant.Config.WizardConfigMapper
import Shared.Util.Swagger

instance ToSchema TenantConfigSubmissionServiceSimple where
  declareNamedSchema = toSwagger (toSubmissionServiceSimple defaultSubmissionService)
