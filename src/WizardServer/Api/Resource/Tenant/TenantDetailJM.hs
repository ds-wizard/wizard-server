module WizardServer.Api.Resource.Tenant.TenantDetailJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageJM ()
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Api.Resource.User.UserJM ()
import Shared.Util.Aeson

instance FromJSON TenantDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDetailDTO where
  toJSON = genericToJSON jsonOptions
