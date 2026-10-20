module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeJM
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelEditorEventsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorEventsDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelEditorEventsDTO where
  jsonSchema = withDefinitions [withProperties [("content", typeSchema "null")] <$> knowledgeModelEntityNodeDefinition] . genericJsonSchema jsonOptions
