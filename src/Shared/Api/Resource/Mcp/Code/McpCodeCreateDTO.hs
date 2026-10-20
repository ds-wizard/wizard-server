module Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO where

import GHC.Generics

import Shared.Model.Mcp.McpToolset

data McpCodeCreateDTO = McpCodeCreateDTO
  { clientId :: String
  , redirectUri :: String
  , codeChallenge :: Maybe String
  , codeChallengeMethod :: Maybe String
  , toolsets :: Maybe [McpToolset]
  }
  deriving (Show, Eq, Generic)
