module Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO where

import Data.Aeson (Value)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectReply

data ProjectDetailQuestionnaireDTO = ProjectDetailQuestionnaireDTO
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , selectedQuestionTagUuids :: [U.UUID]
  , language :: Maybe String
  , locale :: Maybe Value
  , isTemplate :: Bool
  , knowledgeModel :: KnowledgeModel
  , replies :: M.Map String Reply
  , labels :: M.Map String [U.UUID]
  , phaseUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , files :: [ProjectFileSimple]
  , unresolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , resolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
