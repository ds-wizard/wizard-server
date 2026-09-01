module Shared.Model.KnowledgeModel.Event.Answer.AnswerEventUtil where

import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditAnswerEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.aLabel
      , isChangedValue event.advice
      , isChangedValue event.annotations
      , isChangedValue event.metricMeasures
      ]
