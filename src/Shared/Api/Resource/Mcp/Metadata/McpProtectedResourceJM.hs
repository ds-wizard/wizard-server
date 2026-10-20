module Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO

instance ToJSON McpProtectedResourceDTO where
  toJSON McpProtectedResourceDTO {..} =
    object
      [ "resource" .= resource
      , "authorization_servers" .= authorizationServers
      , "bearer_methods_supported" .= bearerMethodsSupported
      , "scopes_supported" .= scopesSupported
      ]
