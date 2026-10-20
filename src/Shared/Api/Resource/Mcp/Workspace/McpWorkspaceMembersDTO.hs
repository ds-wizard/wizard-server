module Shared.Api.Resource.Mcp.Workspace.McpWorkspaceMembersDTO where

import qualified Data.UUID as U
import GHC.Generics

data McpWorkspaceMemberDTO = McpWorkspaceMemberDTO
  { userUuid :: U.UUID
  , roleUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

data McpWorkspaceMembersDTO = McpWorkspaceMembersDTO
  { members :: [McpWorkspaceMemberDTO]
  , removed :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
