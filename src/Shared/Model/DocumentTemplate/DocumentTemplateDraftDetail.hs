module Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Model.Project.ProjectSuggestion

data DocumentTemplateDraftDetail = DocumentTemplateDraftDetail
  { uuid :: U.UUID
  , name :: String
  , templateId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , language :: String
  , formats :: [DocumentTemplateFormat]
  , projectUuid :: Maybe U.UUID
  , project :: Maybe ProjectSuggestion
  , knowledgeModelEditorUuid :: Maybe U.UUID
  , knowledgeModelEditor :: Maybe KnowledgeModelEditorSuggestion
  , formatUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
