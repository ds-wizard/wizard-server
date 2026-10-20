module Shared.Api.Resource.Mcp.Token.McpTokenCreateSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO
import Shared.Api.Resource.Mcp.Token.McpTokenCreateJM ()

instance ToSchema McpTokenCreateDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpTokenCreateDTO") $ mempty
