module Shared.Api.Resource.Mcp.User.McpUserInviteDTO where

import qualified Data.UUID as U
import GHC.Generics

data McpUserInviteDTO = McpUserInviteDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , roleUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
