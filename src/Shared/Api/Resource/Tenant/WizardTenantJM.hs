module Shared.Api.Resource.Tenant.WizardTenantJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.TenantJM ()
import Shared.Util.Aeson

instance FromJSON TenantDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDTO where
  toJSON = genericToJSON jsonOptions
