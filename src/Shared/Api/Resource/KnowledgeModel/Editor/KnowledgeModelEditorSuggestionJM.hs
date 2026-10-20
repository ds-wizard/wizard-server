module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelEditorSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorSuggestion where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelEditorSuggestion where
  jsonSchema = genericJsonSchema jsonOptions
