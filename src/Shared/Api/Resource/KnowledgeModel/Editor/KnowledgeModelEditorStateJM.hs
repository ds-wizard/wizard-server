module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState

instance ToJSON KnowledgeModelEditorState

instance FromJSON KnowledgeModelEditorState
