module Shared.Api.Resource.Tenant.TenantJM where

import Data.Aeson
import Servant.API

import Shared.Api.Resource.Common.FromHttpApiData
import Shared.Model.Tenant.Tenant
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON Tenant where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Tenant where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantState where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantState where
  toJSON = genericToJSON jsonOptions

instance FromHttpApiData [TenantState] where
  parseQueryParam = genericParseQueryParams

instance JsonSchema TenantState where
  jsonSchema = genericJsonSchema jsonOptions
