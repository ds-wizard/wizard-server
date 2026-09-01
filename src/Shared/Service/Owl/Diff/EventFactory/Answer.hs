module Shared.Service.Owl.Diff.EventFactory.Answer where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Service.Owl.Diff.Accessor.Accessor
import Shared.Service.Owl.Diff.EventFactory.EventFactory
import Shared.Util.Uuid

instance EventFactory Answer where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddAnswerEvent' $
              AddAnswerEvent
                { aLabel = entity.aLabel
                , advice = entity.advice
                , annotations = entity.annotations
                , metricMeasures = entity.metricMeasures
                }
        , createdAt = now
        }
  createEditEvent (oldKm, newKm) parentUuid oldEntity newEntity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let event =
          KnowledgeModelEvent
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = newEntity.uuid
            , content =
                EditAnswerEvent' $
                  EditAnswerEvent
                    { aLabel = diffField oldEntity.aLabel newEntity.aLabel
                    , advice = diffField oldEntity.advice newEntity.advice
                    , annotations = diffField oldEntity.annotations newEntity.annotations
                    , followUpUuids =
                        diffListField (oldKm, newKm) oldEntity.followUpUuids newEntity.followUpUuids getQuestionsM
                    , metricMeasures = diffField oldEntity.metricMeasures newEntity.metricMeasures
                    }
            , createdAt = now
            }
    if isEmptyEvent event.content
      then return . Just $ event
      else return Nothing
  createDeleteEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content = DeleteAnswerEvent' DeleteAnswerEvent
        , createdAt = now
        }
