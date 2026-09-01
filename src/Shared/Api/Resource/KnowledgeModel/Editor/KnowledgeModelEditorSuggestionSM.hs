module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorSuggestion where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorSuggestion
