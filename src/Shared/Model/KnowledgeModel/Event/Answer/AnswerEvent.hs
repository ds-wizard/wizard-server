module Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.KnowledgeModel.Hashable ()

data AddAnswerEvent = AddAnswerEvent
  { aLabel :: String
  , advice :: Maybe String
  , annotations :: [MapEntry String String]
  , metricMeasures :: [MetricMeasure]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddAnswerEvent

data EditAnswerEvent = EditAnswerEvent
  { aLabel :: EventField String
  , advice :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  , followUpUuids :: EventField [U.UUID]
  , metricMeasures :: EventField [MetricMeasure]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditAnswerEvent

data DeleteAnswerEvent = DeleteAnswerEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteAnswerEvent
