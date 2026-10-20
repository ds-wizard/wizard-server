module Shared.Api.Resource.Mcp.Token.McpTokenSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Token.McpTokenDTO
import Shared.Api.Resource.Mcp.Token.McpTokenJM ()

instance ToSchema McpTokenDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpTokenDTO") $ mempty
