module Shared.Util.KnowledgeModel.Hashable where

import Data.Hashable

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Hashable ()

instance Hashable MetricMeasure
instance Hashable QuestionValidation
instance Hashable QuestionValueType
instance Hashable a => Hashable (EventField a)
instance (Hashable key, Hashable value) => Hashable (MapEntry key value)
