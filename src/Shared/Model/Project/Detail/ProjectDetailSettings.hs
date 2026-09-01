module Shared.Model.Project.Detail.ProjectDetailSettings where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.Project.Acl.ProjectPermDTO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateState
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectState

data ProjectDetailSettings = ProjectDetailSettings
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , isTemplate :: Bool
  , permissions :: [ProjectPermDTO]
  , projectTags :: [String]
  , knowledgeModelPackageUuid :: U.UUID
  , knowledgeModelPackage :: KnowledgeModelPackageSimpleDTO
  , knowledgeModelTags :: [Tag]
  , knowledgeModelState :: KnowledgeModelProjectState
  , language :: Maybe String
  , availableLocales :: [KnowledgeModelLocaleList]
  , documentTemplate :: Maybe DocumentTemplateSuggestionDTO
  , documentTemplateState :: Maybe DocumentTemplateProjectState
  , documentTemplateSupportState :: Maybe DocumentTemplateState
  , documentTemplatePhase :: Maybe DocumentTemplatePhase
  , formatUuid :: Maybe U.UUID
  , documentTemplateLanguage :: Maybe String
  , selectedQuestionTagUuids :: [U.UUID]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
