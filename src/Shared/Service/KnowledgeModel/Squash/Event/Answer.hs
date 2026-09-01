module Shared.Service.KnowledgeModel.Squash.Event.Answer where

import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditAnswerEvent where
  isSimpleEventSquashApplicable e = not (isChanged (.followUpUuids) e || isChanged (.metricMeasures) e)
  isReorderEventSquashApplicable (previousEvent, _) (newEvent, _) = previousEvent.entityUuid == newEvent.entityUuid
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditAnswerEvent'
        EditAnswerEvent
          { aLabel = applyValue oldContent newContent (.aLabel)
          , advice = applyValue oldContent newContent (.advice)
          , annotations = applyValue oldContent newContent (.annotations)
          , followUpUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.followUpUuids)
          , metricMeasures = applyValue oldContent newContent (.metricMeasures)
          }
