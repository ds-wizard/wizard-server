module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Chapter where

import qualified Data.List as L
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Common.Lens
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Delete
import Shared.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddChapterEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {chapterUuids = km.chapterUuids ++ [event.entityUuid]}
      addEntity = putInChaptersM event.entityUuid (createEntity event content)

instance ApplyEvent EditChapterEvent where
  apply = applyEditEvent getChaptersM setChaptersM

instance ApplyEvent DeleteChapterEvent where
  apply event content = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference km = km {chapterUuids = L.delete event.entityUuid km.chapterUuids}
      deleteEntity km = deleteChapter km event.entityUuid
