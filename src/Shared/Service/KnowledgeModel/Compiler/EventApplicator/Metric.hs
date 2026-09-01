module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Metric where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Common.Lens
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer
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

instance ApplyEvent AddMetricEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {metricUuids = km.metricUuids ++ [event.entityUuid]}
      addEntity = putInMetricsM event.entityUuid (createEntity event content)

instance ApplyEvent EditMetricEvent where
  apply = applyEditEvent getMetricsM setMetricsM

instance ApplyEvent DeleteMetricEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {metricUuids = L.delete event.entityUuid km.metricUuids}
      deleteEntity km = deleteMetric km event.entityUuid
      deleteEntityChildrenReference km =
        setAnswersM km $ M.map (deleteMetricReference event) km.entities.answers
