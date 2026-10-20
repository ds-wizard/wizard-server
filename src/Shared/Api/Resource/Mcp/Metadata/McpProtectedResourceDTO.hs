module Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO where

import GHC.Generics

data McpProtectedResourceDTO = McpProtectedResourceDTO
  { resource :: String
  , authorizationServers :: [String]
  , bearerMethodsSupported :: [String]
  , scopesSupported :: [String]
  }
  deriving (Show, Eq, Generic)
