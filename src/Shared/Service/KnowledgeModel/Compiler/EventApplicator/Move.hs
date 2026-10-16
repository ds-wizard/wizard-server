module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Move where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Move ()

instance ApplyEvent MoveQuestionEvent where
  apply event content km
    | canHoldQuestion km content.targetUuid = Right . moveUnderAnswer . moveUnderQuestion . moveUnderChapter $ km
    | otherwise = Right km
    where
      moveUnderChapter km = setChaptersM km $ M.map (editEntity event content) km.entities.chapters
      moveUnderQuestion km = setQuestionsM km $ M.map (editEntity event content) km.entities.questions
      moveUnderAnswer km = setAnswersM km $ M.map (editEntity event content) km.entities.answers

instance ApplyEvent MoveAnswerEvent where
  apply event content km
    | isQuestion isOptionsQuestion km content.targetUuid = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
    | otherwise = Right km

instance ApplyEvent MoveChoiceEvent where
  apply event content km
    | isQuestion isMultiChoiceQuestion km content.targetUuid = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
    | otherwise = Right km

instance ApplyEvent MoveExpertEvent where
  apply event content km
    | isQuestion (const True) km content.targetUuid = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
    | otherwise = Right km

instance ApplyEvent MoveReferenceEvent where
  apply event content km
    | isQuestion (const True) km content.targetUuid = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
    | otherwise = Right km

canHoldQuestion :: KnowledgeModel -> U.UUID -> Bool
canHoldQuestion km uuid = M.member uuid (getChaptersM km) || M.member uuid (getAnswersM km) || isQuestion isListQuestion km uuid

isQuestion :: (Question -> Bool) -> KnowledgeModel -> U.UUID -> Bool
isQuestion predicate km uuid = maybe False predicate (M.lookup uuid (getQuestionsM km))

isOptionsQuestion :: Question -> Bool
isOptionsQuestion (OptionsQuestion' _) = True
isOptionsQuestion _ = False

isMultiChoiceQuestion :: Question -> Bool
isMultiChoiceQuestion (MultiChoiceQuestion' _) = True
isMultiChoiceQuestion _ = False

isListQuestion :: Question -> Bool
isListQuestion (ListQuestion' _) = True
isListQuestion _ = False
