module Shared.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Util.Aeson

instance FromJSON AddAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditAnswerEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditAnswerEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
