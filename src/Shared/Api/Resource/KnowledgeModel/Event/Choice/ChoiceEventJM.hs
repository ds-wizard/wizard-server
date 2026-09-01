module Shared.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.Util.Aeson

instance FromJSON AddChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditChoiceEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditChoiceEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
