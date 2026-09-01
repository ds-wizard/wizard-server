module Shared.Service.KnowledgeModel.Compiler.Modifier.Metric where

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddMetricEvent Metric where
  createEntity event content =
    Metric
      { uuid = event.entityUuid
      , title = content.title
      , abbreviation = content.abbreviation
      , description = content.description
      , annotations = content.annotations
      }

instance EditEntity EditMetricEvent Metric where
  editEntity event content entity =
    entity
      { title = applyValue entity.title content.title
      , abbreviation = applyValue entity.abbreviation content.abbreviation
      , description = applyValue entity.description content.description
      , annotations = applyValue entity.annotations content.annotations
      }
