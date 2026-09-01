module Shared.Model.KnowledgeModel.Event.Metric.MetricEvent where

import Data.Hashable
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Util.KnowledgeModel.Hashable ()

data AddMetricEvent = AddMetricEvent
  { title :: String
  , abbreviation :: Maybe String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddMetricEvent

data EditMetricEvent = EditMetricEvent
  { title :: EventField String
  , abbreviation :: EventField (Maybe String)
  , description :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditMetricEvent

data DeleteMetricEvent = DeleteMetricEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteMetricEvent
