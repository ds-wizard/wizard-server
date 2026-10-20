module Shared.Api.Resource.Mcp.Tenant.McpTenantAdminJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Tenant.McpTenantAdminDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpTenantAdminDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpTenantAdminDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema McpTenantAdminDTO where
  jsonSchema = genericJsonSchema jsonOptions
