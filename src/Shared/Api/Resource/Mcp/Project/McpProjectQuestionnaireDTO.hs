module Shared.Api.Resource.Mcp.Project.McpProjectQuestionnaireDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Mcp.Project.McpProjectDTO
import Shared.Model.Project.ProjectReply

data McpProjectQuestionnaireDTO = McpProjectQuestionnaireDTO
  { project :: McpProjectDTO
  , phaseUuid :: Maybe U.UUID
  , replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  }
  deriving (Show, Eq, Generic)
