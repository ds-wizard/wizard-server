module Shared.Api.Resource.Mcp.Token.McpTokenDTO where

import GHC.Generics

data McpTokenDTO = McpTokenDTO
  { accessToken :: String
  , tokenType :: String
  , expiresIn :: Integer
  , refreshToken :: String
  }
  deriving (Show, Eq, Generic)
