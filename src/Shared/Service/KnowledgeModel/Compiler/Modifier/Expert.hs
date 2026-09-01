module Shared.Service.KnowledgeModel.Compiler.Modifier.Expert where

import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddExpertEvent Expert where
  createEntity event content =
    Expert
      { uuid = event.entityUuid
      , name = content.name
      , email = content.email
      , annotations = content.annotations
      }

instance EditEntity EditExpertEvent Expert where
  editEntity event content entity =
    entity
      { name = applyValue entity.name content.name
      , email = applyValue entity.email content.email
      , annotations = applyValue entity.annotations content.annotations
      }
