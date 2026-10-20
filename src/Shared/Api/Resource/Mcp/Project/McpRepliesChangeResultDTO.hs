module Shared.Api.Resource.Mcp.Project.McpRepliesChangeResultDTO where

import GHC.Generics

data McpRepliesChangeResultDTO = McpRepliesChangeResultDTO
  { set :: [String]
  , cleared :: [String]
  }
  deriving (Show, Eq, Generic)
