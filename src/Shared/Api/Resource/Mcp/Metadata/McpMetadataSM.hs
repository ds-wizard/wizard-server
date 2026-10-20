module Shared.Api.Resource.Mcp.Metadata.McpMetadataSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO
import Shared.Api.Resource.Mcp.Metadata.McpMetadataJM ()

instance ToSchema McpMetadataDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpMetadataDTO") $ mempty
