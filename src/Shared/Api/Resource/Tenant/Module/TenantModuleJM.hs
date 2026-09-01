module Shared.Api.Resource.Tenant.Module.TenantModuleJM where

import Data.Aeson

import Shared.Model.Tenant.Module.TenantModule
import Shared.Util.Aeson

instance FromJSON TenantModule where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantModule where
  toJSON = genericToJSON jsonOptions
