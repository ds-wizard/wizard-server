module Shared.Service.Owl.Diff.EventFactory.Chapter where

import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventUtil
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Service.Owl.Diff.Accessor.Accessor
import Shared.Service.Owl.Diff.EventFactory.EventFactory
import Shared.Util.Uuid

instance EventFactory Chapter where
  createAddEvent parentUuid entity = do
    eventUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    return $
      KnowledgeModelEvent
        { uuid = eventUuid
        , parentUuid = parentUuid
        , entityUuid = entity.uuid
        , content =
            AddChapterEvent' $
              AddChapterEvent
                { title = entity.title
                , text = entity.text
                , annotations = entity.annotations
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
                EditChapterEvent' $
                  EditChapterEvent
                    { title = diffField oldEntity.title newEntity.title
                    , text = diffField oldEntity.text newEntity.text
                    , annotations = diffField oldEntity.annotations newEntity.annotations
                    , questionUuids =
                        diffListField (oldKm, newKm) oldEntity.questionUuids newEntity.questionUuids getQuestionsM
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
        , content = DeleteChapterEvent' DeleteChapterEvent
        , createdAt = now
        }
