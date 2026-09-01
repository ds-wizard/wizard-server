module Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Util.Aeson

instance FromJSON AddMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
