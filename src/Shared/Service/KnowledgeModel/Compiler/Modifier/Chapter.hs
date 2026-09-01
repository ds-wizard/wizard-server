module Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter where

import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddChapterEvent Chapter where
  createEntity event content =
    Chapter
      { uuid = event.entityUuid
      , title = content.title
      , text = content.text
      , annotations = content.annotations
      , questionUuids = []
      }

instance EditEntity EditChapterEvent Chapter where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , text = applyValue entity.text content.text
      , annotations = applyValue entity.annotations content.annotations
      , questionUuids = applyValue entity.questionUuids content.questionUuids
      }
