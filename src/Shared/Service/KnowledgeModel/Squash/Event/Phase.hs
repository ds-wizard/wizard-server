module Shared.Service.KnowledgeModel.Squash.Event.Phase where

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditPhaseEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditPhaseEvent'
        EditPhaseEvent
          { title = applyValue oldContent newContent (.title)
          , description = applyValue oldContent newContent (.description)
          , annotations = applyValue oldContent newContent (.annotations)
          }
