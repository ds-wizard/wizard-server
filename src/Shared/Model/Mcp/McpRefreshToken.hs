module Shared.Model.Mcp.McpRefreshToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Mcp.McpToolset

data McpRefreshToken = McpRefreshToken
  { uuid :: U.UUID
  , tokenHash :: String
  , previousTokenHash :: Maybe String
  , userTokenUuid :: U.UUID
  , clientId :: String
  , tenantUuid :: U.UUID
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  , toolsets :: [McpToolset]
  }
  deriving (Show, Eq, Generic)
