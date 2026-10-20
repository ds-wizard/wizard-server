module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON AddKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema AddKnowledgeModelEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON EditKnowledgeModelEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditKnowledgeModelEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema EditKnowledgeModelEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")
