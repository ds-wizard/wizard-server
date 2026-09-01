module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM ()
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState

instance ToSchema KnowledgeModelEditorState
