module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Integration where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Common.Lens
import Shared.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Question
import Shared.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddIntegrationEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {integrationUuids = km.integrationUuids ++ [event.entityUuid]}
      addEntity = putInIntegrationsM event.entityUuid (createEntity event content)

instance ApplyEvent EditIntegrationEvent where
  apply event content = updateIntegrationVariables' content . applyEditEvent getIntegrationsM setIntegrationsM event content
    where
      updateIntegrationVariables' :: EditIntegrationEvent -> Either AppError KnowledgeModel -> Either AppError KnowledgeModel
      updateIntegrationVariables' _ (Left error) = Left error
      updateIntegrationVariables' content (Right km) = Right . setQuestionsM km $ M.map (updateIntegrationVariables event content) km.entities.questions

instance ApplyEvent DeleteIntegrationEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {integrationUuids = L.delete event.entityUuid km.integrationUuids}
      deleteEntity km = setIntegrationsM km $ M.delete event.entityUuid (getIntegrationsM km)
      deleteEntityChildrenReference km =
        setQuestionsM km $ M.map (deleteIntegrationReference event) km.entities.questions
