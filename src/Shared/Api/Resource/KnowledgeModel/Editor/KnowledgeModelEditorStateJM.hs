module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelEditorState

instance FromJSON KnowledgeModelEditorState

instance JsonSchema KnowledgeModelEditorState where
  jsonSchema = genericJsonSchema defaultOptions
