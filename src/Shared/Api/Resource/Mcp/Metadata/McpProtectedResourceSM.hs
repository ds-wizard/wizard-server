module Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceDTO
import Shared.Api.Resource.Mcp.Metadata.McpProtectedResourceJM ()

instance ToSchema McpProtectedResourceDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpProtectedResourceDTO") $ mempty
