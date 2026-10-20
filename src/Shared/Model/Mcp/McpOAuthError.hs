module Shared.Model.Mcp.McpOAuthError where

import GHC.Generics

data McpOAuthError = McpOAuthError
  { status :: Int
  , errorCode :: String
  , errorDescription :: String
  }
  deriving (Show, Eq, Generic)

invalidRequest :: String -> McpOAuthError
invalidRequest = McpOAuthError 400 "invalid_request"

invalidClient :: String -> McpOAuthError
invalidClient = McpOAuthError 401 "invalid_client"

invalidGrant :: String -> McpOAuthError
invalidGrant = McpOAuthError 400 "invalid_grant"

unsupportedGrantType :: String -> McpOAuthError
unsupportedGrantType grantType = McpOAuthError 400 "unsupported_grant_type" ("Unsupported grant_type: " ++ grantType)

invalidClientMetadata :: String -> McpOAuthError
invalidClientMetadata = McpOAuthError 400 "invalid_client_metadata"

invalidRedirectUri :: String -> McpOAuthError
invalidRedirectUri = McpOAuthError 400 "invalid_redirect_uri"

unknownClient :: String -> McpOAuthError
unknownClient clientId = McpOAuthError 404 "invalid_client" ("Client '" ++ clientId ++ "' does not exist")
