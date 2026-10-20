module Shared.Api.Resource.Mcp.Project.McpRepliesChangeDTO where

import qualified Data.Map.Strict as M

import Shared.Model.Project.ProjectReply

data McpRepliesChangeDTO = McpRepliesChangeDTO
  { replies :: M.Map String ReplyValue
  , clear :: [String]
  }
  deriving (Show, Eq)
