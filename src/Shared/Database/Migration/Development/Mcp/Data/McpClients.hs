module Shared.Database.Migration.Development.Mcp.Data.McpClients where

import Shared.Constant.Tenant
import Shared.Model.Mcp.McpClient
import Shared.Util.Date
import Shared.Util.Uuid

mcpClient :: McpClient
mcpClient =
  McpClient
    { uuid = u' "5d7c6a32-1f4e-4b8a-9c2d-7e1f3a5b9c40"
    , clientId = "audit-log-client"
    , clientSecret = "audit-log-secret"
    , clientName = Just "Claude"
    , redirectUris = [mcpRedirectUri]
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2026 1 1
    , updatedAt = dt' 2026 1 1
    , tokenEndpointAuthMethod = "client_secret_post"
    }

mcpPublicClient :: McpClient
mcpPublicClient =
  mcpClient
    { uuid = u' "7b1e9d4c-2a6f-4c8b-8e3d-1f5a7c9b2d61"
    , clientId = "public-client"
    , clientSecret = ""
    , tokenEndpointAuthMethod = "none"
    }

mcpRedirectUri :: String
mcpRedirectUri = "https://claude.ai/api/mcp/auth_callback"
