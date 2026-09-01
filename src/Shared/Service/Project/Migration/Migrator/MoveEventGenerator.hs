module Shared.Service.Project.Migration.Migrator.MoveEventGenerator where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (id)

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelUtil

generateEvents :: UTCTime -> KnowledgeModel -> KnowledgeModel -> [KnowledgeModelEvent]
generateEvents now oldKm newKm = moveQuestionEvents ++ moveAnswerEvents
  where
    moveQuestionEvents = foldl (generateQuestionMoveEvent now oldParentMap newParentMap) [] (getQuestionsL newKm)
    moveAnswerEvents = foldl (generateAnswerMoveEvents now oldParentMap newParentMap) [] (getAnswersL newKm)
    oldParentMap = makeParentMap oldKm
    newParentMap = makeParentMap newKm

generateQuestionMoveEvent :: UTCTime -> KMParentMap -> KMParentMap -> [KnowledgeModelEvent] -> Question -> [KnowledgeModelEvent]
generateQuestionMoveEvent now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (getUuid entity) oldParentMap
    newParentUuid <- M.lookup (getUuid entity) newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          KnowledgeModelEvent
            { uuid = U.nil
            , parentUuid = oldParentUuid
            , entityUuid = getUuid entity
            , content =
                MoveQuestionEvent'
                  MoveQuestionEvent {targetUuid = newParentUuid}
            , createdAt = now
            }
    return $ events ++ [event]

generateAnswerMoveEvents :: UTCTime -> KMParentMap -> KMParentMap -> [KnowledgeModelEvent] -> Answer -> [KnowledgeModelEvent]
generateAnswerMoveEvents now oldParentMap newParentMap events entity =
  fromMaybe events $ do
    oldParentUuid <- M.lookup (getUuid entity) oldParentMap
    newParentUuid <- M.lookup (getUuid entity) newParentMap
    _ <- guard $ newParentUuid /= oldParentUuid
    let event =
          KnowledgeModelEvent
            { uuid = U.nil
            , parentUuid = oldParentUuid
            , entityUuid = getUuid entity
            , content =
                MoveAnswerEvent' $
                  MoveAnswerEvent
                    { targetUuid = newParentUuid
                    }
            , createdAt = now
            }
    return $ events ++ [event]
