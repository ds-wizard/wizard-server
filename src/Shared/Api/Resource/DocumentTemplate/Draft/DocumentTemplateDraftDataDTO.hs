module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Model.Project.ProjectSuggestion

data DocumentTemplateDraftDataDTO = DocumentTemplateDraftDataDTO
  { projectUuid :: Maybe U.UUID
  , project :: Maybe ProjectSuggestion
  , knowledgeModelEditorUuid :: Maybe U.UUID
  , knowledgeModelEditor :: Maybe KnowledgeModelEditorSuggestion
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
