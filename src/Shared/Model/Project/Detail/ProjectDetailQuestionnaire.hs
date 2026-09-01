module Shared.Model.Project.Detail.ProjectDetailQuestionnaire where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project

data ProjectDetailQuestionnaire = ProjectDetailQuestionnaire
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , selectedQuestionTagUuids :: [U.UUID]
  , language :: Maybe String
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , files :: [ProjectFileSimple]
  }
  deriving (Show, Eq, Generic)
