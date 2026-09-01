module Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Common.Lens
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Choice ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Question ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Shared.Service.KnowledgeModel.Compiler.Modifier.Tag ()

_NODE_NOT_FOUND nodeType node =
  "Node (type: " ++ nodeType ++ ", uuid: " ++ U.toString node.entityUuid ++ ") was not found"

class ApplyEvent a where
  apply :: KnowledgeModelEvent -> a -> KnowledgeModel -> Either AppError KnowledgeModel

applyCreateEventWithParent getEntityCol setEntityCol getParentCol setParentCol getParentUuidCol setParentUuidCol event content km =
  case M.lookup event.parentUuid (getParentCol km) of
    Nothing -> Right km
    Just parentEntity -> Right . addEntityReference parentEntity . addEntity $ km
  where
    addEntityReference entity km = setParentCol km $ M.insert (getUuid entity) (setParentUuidCol entity (getParentUuidCol entity ++ [event.entityUuid])) (getParentCol km)
    addEntity km = setEntityCol km $ M.insert event.entityUuid (createEntity event content) (getEntityCol km)

applyEditEvent getEntityCol setEntityCol event content km =
  case M.lookup event.entityUuid (getEntityCol km) of
    Nothing -> Right km
    Just entity -> Right . updateEntity km $ entity
  where
    updateEntity km entity =
      setEntityCol km $ M.insert event.entityUuid (editEntity event content entity) (getEntityCol km)

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
deleteEntityReferenceFromParentNode event getParentCollectionInEntities setParentCollectionInEntities getParentCollectionOfChildUuids setParentCollectionOfChildUuids km =
  case M.lookup event.parentUuid (getParentCollectionInEntities km) of
    Nothing -> Right km
    Just parentNode -> Right . removeEntityReference $ parentNode
  where
    removeEntityReference parentNode =
      setParentCollectionInEntities km $ M.insert (getUuid parentNode) (setParentCollectionOfChildUuids parentNode $ L.delete event.entityUuid (getParentCollectionOfChildUuids parentNode)) (getParentCollectionInEntities km)
