module Shared.Model.Project.Detail.ProjectDetail where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.Project

data ProjectDetail = ProjectDetail
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
