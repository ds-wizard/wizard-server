module Shared.Api.Resource.Mcp.Project.McpProjectDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Project.Project

data McpProjectDTO = McpProjectDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageUuid :: U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , projectTags :: [String]
  , language :: Maybe String
  , documentTemplateUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , documentTemplateLanguage :: Maybe String
  , isTemplate :: Bool
  , workspaceUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
