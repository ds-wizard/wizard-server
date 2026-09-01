module WizardServer.Api.Resource.Tenant.TenantCreateJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Util.Aeson

instance FromJSON TenantCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantCreateDTO where
  toJSON = genericToJSON jsonOptions
