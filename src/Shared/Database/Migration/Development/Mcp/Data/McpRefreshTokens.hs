module Shared.Database.Migration.Development.Mcp.Data.McpRefreshTokens where

import Shared.Constant.Tenant
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Database.Migration.Development.User.Data.UserTokens
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.UserToken
import Shared.Util.Date
import Shared.Util.Uuid

mcpGrantToken :: UserToken
mcpGrantToken =
  albertMcpToken
    { uuid = u' "c5a3e1d7-9f2b-4c6e-8d0a-4b6e8f0a2c35"
    , value = "mcp-grant-access-token"
    , expiresAt = dt' 2027 1 1
    }

mcpRefreshTokenValue :: String
mcpRefreshTokenValue = "mcp-refresh-token-value"

mcpRefreshTokenHash :: String
mcpRefreshTokenHash = "76ab0ade3159fcce46593f2e784c15b396ea95d767052a77bbd4f5540aa9f1bb"

mcpGrantRefreshToken :: McpRefreshToken
mcpGrantRefreshToken =
  McpRefreshToken
    { uuid = u' "d9b7f5e3-1a2c-4d6e-8f0b-3c5e7a9d1f46"
    , tokenHash = mcpRefreshTokenHash
    , previousTokenHash = Nothing
    , userTokenUuid = mcpGrantToken.uuid
    , clientId = mcpClient.clientId
    , tenantUuid = defaultTenantUuid
    , expiresAt = dt' 2100 1 1
    , createdAt = dt' 2026 1 1
    , toolsets = [KmMcpToolset, DtMcpToolset]
    }

mcpExpiredGrantRefreshToken :: McpRefreshToken
mcpExpiredGrantRefreshToken = mcpGrantRefreshToken {uuid = u' "1f5b7d9e-3a2c-4c6e-9b0d-5e7a9c1f3b58", expiresAt = dt' 2020 1 1}

mcpRotatedRefreshToken :: McpRefreshToken
mcpRotatedRefreshToken =
  mcpGrantRefreshToken
    { uuid = u' "e0c8a6f4-2b3d-4e7f-9a1c-4d6f8b0e2a57"
    , tokenHash = "rotated-refresh-token-hash"
    , previousTokenHash = Just mcpRefreshTokenHash
    }

albertMcpRefreshToken :: McpRefreshToken
albertMcpRefreshToken =
  mcpGrantRefreshToken
    { uuid = u' "6c9e1a3d-5f7b-4e2c-8a4d-0b2f4e6a8c19"
    , tokenHash = "albert-mcp-refresh-token-hash"
    , userTokenUuid = albertMcpToken.uuid
    }

mcpLiveRefreshToken :: McpRefreshToken
mcpLiveRefreshToken =
  mcpGrantRefreshToken
    { uuid = u' "2a8d4c6e-0f1b-4d3e-9a5c-7b9e1f3d5a72"
    , tokenHash = "refresh-token-hash"
    , userTokenUuid = albertToken.uuid
    }
