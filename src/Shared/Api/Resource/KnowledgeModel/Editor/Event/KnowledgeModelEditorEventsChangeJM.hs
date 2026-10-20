module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM where

import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM (knowledgeModelEventTypes)
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelEditorEventsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorEventsChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelEditorEventChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorEventChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelEditorEventChangeDTO where
  jsonSchema =
    withProperties
      [ ("parentUuid", describedSchema "The parent entity: a UUID, or a $name created earlier in the batch" stringSchema)
      , ("entityUuid", describedSchema "The entity the event creates or changes: a UUID, or a $name for an entity it creates" stringSchema)
      , ("content", describedSchema "The event: eventType and the fields of that type (describe_knowledge_model_events)" eventContentSchema)
      , ("position", describedSchema "The index of the entity among the children of its parent after the event, from 0" (jsonSchema (Proxy :: Proxy (Maybe Int))))
      ]
      . genericJsonSchema jsonOptions

eventContentSchema :: Value
eventContentSchema = object ["type" .= ("object" :: String), "properties" .= object ["eventType" .= enumSchema knowledgeModelEventTypes], "required" .= ["eventType" :: String]]
