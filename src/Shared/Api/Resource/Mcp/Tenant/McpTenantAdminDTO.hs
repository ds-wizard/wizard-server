module Shared.Api.Resource.Mcp.Tenant.McpTenantAdminDTO where

import GHC.Generics

data McpTenantAdminDTO = McpTenantAdminDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  }
  deriving (Generic)
