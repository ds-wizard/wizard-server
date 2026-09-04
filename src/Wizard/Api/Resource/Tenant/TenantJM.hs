module Wizard.Api.Resource.Tenant.TenantJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.TenantDTO
import WizardLib.Public.Api.Resource.Tenant.TenantJM ()

instance FromJSON TenantDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDTO where
  toJSON = genericToJSON jsonOptions
