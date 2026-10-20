module Shared.Api.Resource.Mcp.Code.McpCodeSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Code.McpCodeDTO
import Shared.Api.Resource.Mcp.Code.McpCodeJM ()

instance ToSchema McpCodeDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpCodeDTO") $ mempty
