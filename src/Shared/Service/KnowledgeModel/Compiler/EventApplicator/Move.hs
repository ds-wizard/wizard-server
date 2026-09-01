module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Move where

import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Shared.Service.KnowledgeModel.Compiler.Modifier.Move ()

instance ApplyEvent MoveQuestionEvent where
  apply event content = Right . moveUnderAnswer . moveUnderQuestion . moveUnderChapter
    where
      moveUnderChapter km = setChaptersM km $ M.map (editEntity event content) km.entities.chapters
      moveUnderQuestion km = setQuestionsM km $ M.map (editEntity event content) km.entities.questions
      moveUnderAnswer km = setAnswersM km $ M.map (editEntity event content) km.entities.answers

instance ApplyEvent MoveAnswerEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveChoiceEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveExpertEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions

instance ApplyEvent MoveReferenceEvent where
  apply event content km = Right . setQuestionsM km $ M.map (editEntity event content) km.entities.questions
