module Shared.Api.Resource.KnowledgeModel.Event.Move.MoveEventJM where

import Data.Aeson

import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON MoveQuestionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveQuestionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema MoveQuestionEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema MoveAnswerEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema MoveChoiceEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema MoveExpertEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

instance FromJSON MoveReferenceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON MoveReferenceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema MoveReferenceEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")
