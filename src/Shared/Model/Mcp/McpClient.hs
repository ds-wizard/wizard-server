module Shared.Model.Mcp.McpClient where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data McpClient = McpClient
  { uuid :: U.UUID
  , clientId :: String
  , clientSecret :: String
  , clientName :: Maybe String
  , redirectUris :: [String]
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , tokenEndpointAuthMethod :: String
  }
  deriving (Show, Eq, Generic)
