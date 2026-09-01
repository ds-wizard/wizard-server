module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Expert where

import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Common.Lens
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Delete
import Shared.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddExpertEvent where
  apply = applyCreateEventWithParent getExpertsM setExpertsM getQuestionsM setQuestionsM getExpertUuids setExpertUuids

instance ApplyEvent EditExpertEvent where
  apply = applyEditEvent getExpertsM setExpertsM

instance ApplyEvent DeleteExpertEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getQuestionsM setQuestionsM getExpertUuids setExpertUuids $ deleteExpert km event.entityUuid
