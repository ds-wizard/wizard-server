module Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Util.Swagger

instance ToSchema AddMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeleteMetricEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
