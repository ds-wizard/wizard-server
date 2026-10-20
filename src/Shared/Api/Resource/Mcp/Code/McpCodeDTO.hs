module Shared.Api.Resource.Mcp.Code.McpCodeDTO where

import GHC.Generics

newtype McpCodeDTO = McpCodeDTO
  { code :: String
  }
  deriving (Show, Eq, Generic)
