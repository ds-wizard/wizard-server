module Shared.Model.KnowledgeModel.KnowledgeModelUtil where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

isItListQuestion :: KnowledgeModel -> U.UUID -> Bool
isItListQuestion km itemUuid =
  case M.lookup itemUuid (getQuestionsM km) of
    Just (ListQuestion' _) -> True
    _ -> False

entityExists :: KnowledgeModel -> U.UUID -> Bool
entityExists km = isJust . findEntity km

getChildLists :: KnowledgeModel -> U.UUID -> [(String, [U.UUID])]
getChildLists km = maybe [] (entityChildLists km) . findEntity km

entityChildLists :: KnowledgeModel -> KnowledgeModelEntity -> [(String, [U.UUID])]
entityChildLists km entity =
  case entity of
    KnowledgeModelEntity -> knowledgeModelChildLists km
    ChapterEntity chapter -> [("questionUuids", chapter.questionUuids)]
    QuestionEntity question -> questionChildLists question
    AnswerEntity answer -> [("followUpUuids", answer.followUpUuids)]
    ResourceCollectionEntity collection -> [("resourcePageUuids", collection.resourcePageUuids)]
    _ -> []

knowledgeModelChildLists :: KnowledgeModel -> [(String, [U.UUID])]
knowledgeModelChildLists km =
  [ ("chapterUuids", km.chapterUuids)
  , ("tagUuids", km.tagUuids)
  , ("integrationUuids", km.integrationUuids)
  , ("metricUuids", km.metricUuids)
  , ("phaseUuids", km.phaseUuids)
  , ("resourceCollectionUuids", km.resourceCollectionUuids)
  ]

questionChildLists :: Question -> [(String, [U.UUID])]
questionChildLists question = typeSpecific question ++ [("expertUuids", getExpertUuids question), ("referenceUuids", getReferenceUuids question)]
  where
    typeSpecific (OptionsQuestion' q) = [("answerUuids", q.answerUuids)]
    typeSpecific (MultiChoiceQuestion' q) = [("choiceUuids", q.choiceUuids)]
    typeSpecific (ListQuestion' q) = [("itemTemplateQuestionUuids", q.itemTemplateQuestionUuids)]
    typeSpecific _ = []

getChildUuids :: KnowledgeModel -> U.UUID -> [U.UUID]
getChildUuids km = concatMap snd . getChildLists km

makeParentMap :: KnowledgeModel -> KMParentMap
makeParentMap km = M.fromList [(child, parent) | parent <- parents, child <- getChildUuids km parent]
  where
    parents =
      km.uuid
        : M.keys km.entities.chapters
        ++ M.keys km.entities.questions
        ++ M.keys km.entities.answers
        ++ M.keys km.entities.resourceCollections

getAncestorUuids :: KMParentMap -> U.UUID -> [U.UUID]
getAncestorUuids parentMap = take (M.size parentMap) . go
  where
    go uuid = maybe [] (\parent -> parent : go parent) (M.lookup uuid parentMap)
