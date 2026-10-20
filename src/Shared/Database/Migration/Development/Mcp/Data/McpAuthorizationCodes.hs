module Shared.Database.Migration.Development.Mcp.Data.McpAuthorizationCodes where

import Shared.Constant.Tenant
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.User
import Shared.Util.Crypto (hashSHA256)
import Shared.Util.Date
import Shared.Util.Uuid

mcpAuthorizationCodeValue :: String
mcpAuthorizationCodeValue = "audit-log-code"

mcpAuthorizationCode :: McpAuthorizationCode
mcpAuthorizationCode =
  McpAuthorizationCode
    { uuid = u' "0f6b1d3e-8a2c-4e5f-9b7d-3c1e5a7f9b20"
    , code = hashSHA256 mcpAuthorizationCodeValue
    , clientId = mcpClient.clientId
    , userUuid = userAlbert.uuid
    , redirectUri = mcpRedirectUri
    , expiresAt = dt' 2100 1 1
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2026 1 1
    , codeChallenge = Nothing
    , codeChallengeMethod = Nothing
    , toolsets = allMcpToolsets
    }

mcpToolsetsCodeValue :: String
mcpToolsetsCodeValue = "toolsets-code"

mcpToolsetsAuthorizationCode :: McpAuthorizationCode
mcpToolsetsAuthorizationCode =
  mcpAuthorizationCode
    { uuid = u' "5b8e2d4f-7a1c-4f3e-9d6b-1e3a5c7f9b02"
    , code = hashSHA256 mcpToolsetsCodeValue
    , toolsets = [KmMcpToolset, DtMcpToolset]
    }

mcpExpiredCodeValue :: String
mcpExpiredCodeValue = "expired-code"

mcpExpiredAuthorizationCode :: McpAuthorizationCode
mcpExpiredAuthorizationCode =
  mcpAuthorizationCode
    { uuid = u' "9c3e5a7f-1b2d-4e6f-8a9b-0c1d2e3f4a51"
    , code = hashSHA256 mcpExpiredCodeValue
    , expiresAt = dt' 2020 1 1
    }

mcpPkceVerifier :: String
mcpPkceVerifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

mcpPkceChallenge :: String
mcpPkceChallenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

mcpPkceCodeValue :: String
mcpPkceCodeValue = "pkce-code"

mcpPkceAuthorizationCode :: McpAuthorizationCode
mcpPkceAuthorizationCode =
  mcpAuthorizationCode
    { uuid = u' "4e7a1c9b-5d2f-4b8e-a6c3-9f1d3b5e7a92"
    , code = hashSHA256 mcpPkceCodeValue
    , codeChallenge = Just mcpPkceChallenge
    , codeChallengeMethod = Just "S256"
    }

mcpPublicCodeValue :: String
mcpPublicCodeValue = "public-code"

mcpPublicAuthorizationCode :: McpAuthorizationCode
mcpPublicAuthorizationCode =
  mcpAuthorizationCode
    { uuid = u' "8c2d6e0a-3b7f-4d1c-9e5a-2b4f6d8c0e13"
    , code = hashSHA256 mcpPublicCodeValue
    , clientId = mcpPublicClient.clientId
    }

mcpPublicPkceCodeValue :: String
mcpPublicPkceCodeValue = "public-pkce-code"

mcpPublicPkceAuthorizationCode :: McpAuthorizationCode
mcpPublicPkceAuthorizationCode =
  mcpPublicAuthorizationCode
    { uuid = u' "1f6b3d8e-9c4a-4e2b-b7d5-3a9c1e5f7b24"
    , code = hashSHA256 mcpPublicPkceCodeValue
    , codeChallenge = Just mcpPkceChallenge
    , codeChallengeMethod = Just "S256"
    }
