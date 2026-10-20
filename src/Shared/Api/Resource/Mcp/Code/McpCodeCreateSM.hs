module Shared.Api.Resource.Mcp.Code.McpCodeCreateSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Code.McpCodeCreateJM ()

instance ToSchema McpCodeCreateDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpCodeCreateDTO") $ mempty
