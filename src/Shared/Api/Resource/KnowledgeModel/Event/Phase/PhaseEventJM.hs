module Shared.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.Util.Aeson

instance FromJSON AddPhaseEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddPhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditPhaseEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditPhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
