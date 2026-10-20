module Shared.Api.Resource.Mcp.Client.McpClientDTO where

import GHC.Generics

data McpClientDTO = McpClientDTO
  { clientName :: Maybe String
  , redirectUris :: [String]
  }
  deriving (Show, Eq, Generic)
