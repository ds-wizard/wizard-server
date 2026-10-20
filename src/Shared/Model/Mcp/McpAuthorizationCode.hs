module Shared.Model.Mcp.McpAuthorizationCode where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Mcp.McpToolset

data McpAuthorizationCode = McpAuthorizationCode
  { uuid :: U.UUID
  , code :: String
  , clientId :: String
  , userUuid :: U.UUID
  , redirectUri :: String
  , expiresAt :: UTCTime
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , codeChallenge :: Maybe String
  , codeChallengeMethod :: Maybe String
  , toolsets :: [McpToolset]
  }
  deriving (Show, Eq, Generic)
