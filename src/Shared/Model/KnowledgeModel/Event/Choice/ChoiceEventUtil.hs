module Shared.Model.KnowledgeModel.Event.Choice.ChoiceEventUtil where

import Shared.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance IsEmptyEvent EditChoiceEvent where
  isEmptyEvent event = or [isChangedValue event.aLabel, isChangedValue event.annotations]
