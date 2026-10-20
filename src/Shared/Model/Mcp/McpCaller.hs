module Shared.Model.Mcp.McpCaller where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.Mcp.McpToolset

data McpCaller = McpCaller
  { tokenUuid :: Maybe U.UUID
  , grant :: Maybe McpRefreshToken
  , toolsets :: [McpToolset]
  }
  deriving (Show, Eq, Generic)
