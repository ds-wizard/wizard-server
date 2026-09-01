module WizardServer.Api.Resource.Tenant.TenantChangeJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Util.Aeson

instance FromJSON TenantChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantChangeDTO where
  toJSON = genericToJSON jsonOptions
