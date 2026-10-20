module WizardServer.Api.Resource.Tenant.TenantDetailJM where

import Data.Aeson

import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Api.Resource.User.UserJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON TenantDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDetailDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema TenantDetailDTO where
  jsonSchema = genericJsonSchema jsonOptions
