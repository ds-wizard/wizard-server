module Shared.Service.KnowledgeModel.Squash.Event.Tag where

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditTagEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False
  isTypeChanged _ _ = False
  simpleSquashEvent mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) =
    createSquashedEvent oldEvent newEvent $
      EditTagEvent'
        EditTagEvent
          { name = applyValue oldContent newContent (.name)
          , description = applyValue oldContent newContent (.description)
          , color = applyValue oldContent newContent (.color)
          , annotations = applyValue oldContent newContent (.annotations)
          }
