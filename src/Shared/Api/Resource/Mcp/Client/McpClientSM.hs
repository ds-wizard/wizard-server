module Shared.Api.Resource.Mcp.Client.McpClientSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Client.McpClientDTO
import Shared.Api.Resource.Mcp.Client.McpClientJM ()

instance ToSchema McpClientDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpClientDTO") $ mempty
