module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Shared.Util.Aeson

instance FromJSON KnowledgeModelEditorSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorSuggestion where
  toJSON = genericToJSON jsonOptions
