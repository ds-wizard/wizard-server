module Shared.Api.Resource.Mcp.Project.McpProjectDetailDTO where

import GHC.Generics

import Shared.Api.Resource.Mcp.Project.McpProjectDTO
import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.Project.ProjectState

data McpProjectDetailDTO = McpProjectDetailDTO
  { project :: McpProjectDTO
  , permissions :: [ProjectPermDTO]
  , knowledgeModelState :: KnowledgeModelProjectState
  , documentTemplateState :: Maybe DocumentTemplateProjectState
  }
  deriving (Show, Eq, Generic)
