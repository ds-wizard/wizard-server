module Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON AddMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema AddMetricEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditMetricEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditMetricEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance JsonSchema EditMetricEvent where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "eventType")
