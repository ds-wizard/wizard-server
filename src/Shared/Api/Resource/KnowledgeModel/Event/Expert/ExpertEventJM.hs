module Shared.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Util.Aeson

instance FromJSON AddExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditExpertEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditExpertEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
