module Shared.Service.KnowledgeModel.Compiler.EventApplicator.Question where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.KnowledgeModel.Common.Lens
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Shared.Service.KnowledgeModel.Compiler.Modifier.Answer ()
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

instance ApplyEvent AddQuestionEvent where
  apply event content km = Right . maybe km addEntity . addEntityReference $ km
    where
      addEntityReference :: KnowledgeModel -> Maybe KnowledgeModel
      addEntityReference km =
        case M.lookup event.parentUuid (getChaptersM km) of
          Just parent ->
            Just $ putInChaptersM event.parentUuid (parent {questionUuids = parent.questionUuids ++ [event.entityUuid]}) km
          Nothing ->
            case M.lookup event.parentUuid (getQuestionsM km) of
              Just parent@(ListQuestion' _) ->
                Just $ putInQuestionsM event.parentUuid (setItemTemplateQuestionUuids parent (getItemTemplateQuestionUuids parent ++ [event.entityUuid])) km
              Just _ -> Nothing
              Nothing ->
                case M.lookup event.parentUuid (getAnswersM km) of
                  Just parent ->
                    Just $ putInAnswersM event.parentUuid (parent {followUpUuids = parent.followUpUuids ++ [event.entityUuid]}) km
                  Nothing -> Nothing
      addEntity :: KnowledgeModel -> KnowledgeModel
      addEntity = putInQuestionsM event.entityUuid (createEntity event content)

instance ApplyEvent EditQuestionEvent where
  apply event content km =
    case M.lookup event.entityUuid (getQuestionsM km) of
      Nothing -> Right km
      Just question ->
        let editedQuestion = editEntity event content question
         in Right . putInQuestionsM event.entityUuid editedQuestion . deleteUnfittingChildren question editedQuestion $ km

deleteUnfittingChildren :: Question -> Question -> KnowledgeModel -> KnowledgeModel
deleteUnfittingChildren question editedQuestion = deleteItemTemplateQuestions . deleteChoices . deleteAnswers
  where
    deleteAnswers km =
      case editedQuestion of
        OptionsQuestion' _ -> km
        _ -> foldl deleteAnswer km (getAnswerUuids question)
    deleteChoices km =
      case editedQuestion of
        MultiChoiceQuestion' _ -> km
        _ -> foldl deleteChoice km (getChoiceUuids question)
    deleteItemTemplateQuestions km =
      case editedQuestion of
        ListQuestion' _ -> km
        _ -> foldl deleteQuestion km (getItemTemplateQuestionUuids question)

instance ApplyEvent DeleteQuestionEvent where
  apply event content = Right . deleteEntity . deleteEntityReference
    where
      deleteEntityReference :: KnowledgeModel -> KnowledgeModel
      deleteEntityReference km =
        case M.lookup event.parentUuid (getChaptersM km) of
          Just parent ->
            putInChaptersM event.parentUuid (parent {questionUuids = L.delete event.entityUuid parent.questionUuids}) km
          Nothing ->
            case M.lookup event.parentUuid (getQuestionsM km) of
              Just parent ->
                putInQuestionsM event.parentUuid (setItemTemplateQuestionUuids parent (L.delete event.entityUuid (getItemTemplateQuestionUuids parent))) km
              Nothing ->
                case M.lookup event.parentUuid (getAnswersM km) of
                  Just parent -> putInAnswersM event.parentUuid (parent {followUpUuids = L.delete event.entityUuid parent.followUpUuids}) km
                  Nothing -> km
      deleteEntity :: KnowledgeModel -> KnowledgeModel
      deleteEntity km = deleteQuestion km event.entityUuid
