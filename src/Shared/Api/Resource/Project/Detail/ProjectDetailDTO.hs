module Shared.Api.Resource.Project.Detail.ProjectDetailDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.Project

data ProjectDetailDTO = ProjectDetailDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: ProjectSharing
  , visibility :: ProjectVisibility
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
