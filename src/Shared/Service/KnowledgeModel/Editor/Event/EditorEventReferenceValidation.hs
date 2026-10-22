module Shared.Service.KnowledgeModel.Editor.Event.EditorEventReferenceValidation where

import qualified Data.List as L
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.UUID as U

import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.Localization.LocaleRecord
import Shared.Service.KnowledgeModel.Editor.Event.EditorEventStructureValidation

data EntityReference = EntityReference
  { field :: String
  , kind :: EntityKind
  , uuids :: [U.UUID]
  }
  deriving (Show, Eq)

validateEventReferences :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateEventReferences km nextKm event = validateReferences km nextKm event.entityUuid ++ validateChildLists km nextKm event

validateReferences :: KnowledgeModel -> KnowledgeModel -> U.UUID -> [(String, LocaleRecord)]
validateReferences km nextKm uuid =
  concatMap (validateReference nextKm) . filter (`notElem` getEntityReferences km uuid) $ getEntityReferences nextKm uuid

validateReference :: KnowledgeModel -> EntityReference -> [(String, LocaleRecord)]
validateReference km reference =
  [ ("content." ++ reference.field, absenceError uuid [reference.kind])
  | uuid <- reference.uuids
  , uuid /= U.nil
  , not (isOfEntityKind km reference.kind uuid)
  ]

validateChildLists :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateChildLists km nextKm event
  | isExistingEntityEvent event.content = concatMap validateChildList (getChildLists nextKm event.entityUuid)
  | otherwise = []
  where
    currentChildLists = getChildLists km event.entityUuid
    validateChildList (field, uuids) =
      let currentUuids = fromMaybe [] (L.lookup field currentChildLists)
       in [("content." ++ field, permutationError currentUuids) | L.sort uuids /= L.sort currentUuids]

permutationError :: [U.UUID] -> LocaleRecord
permutationError = _ERROR_VALIDATION__KM_EVENT_CHILDREN_PERMUTATION . L.intercalate ", " . fmap U.toString

getEntityReferences :: KnowledgeModel -> U.UUID -> [EntityReference]
getEntityReferences km uuid =
  case findEntity km uuid of
    Just (QuestionEntity question) -> questionReferences question
    Just (AnswerEntity answer) -> answerReferences answer
    Just (ReferenceEntity reference) -> referenceReferences reference
    _ -> []

questionReferences :: Question -> [EntityReference]
questionReferences question =
  [ EntityReference "tagUuids" TagEntityKind (getTagUuids question)
  , EntityReference "requiredPhaseUuid" PhaseEntityKind (maybeToList (getRequiredPhaseUuid question))
  ]
    ++ typeSpecific question
  where
    typeSpecific (IntegrationQuestion' q) = [EntityReference "integrationUuid" IntegrationEntityKind [q.integrationUuid]]
    typeSpecific (ItemSelectQuestion' q) = [EntityReference "listQuestionUuid" ListQuestionEntityKind (maybeToList q.listQuestionUuid)]
    typeSpecific _ = []

answerReferences :: Answer -> [EntityReference]
answerReferences answer = [EntityReference "metricMeasures" MetricEntityKind (fmap (.metricUuid) answer.metricMeasures)]

referenceReferences :: Reference -> [EntityReference]
referenceReferences (ResourcePageReference' reference) = [EntityReference "resourcePageUuid" ResourcePageEntityKind (maybeToList reference.resourcePageUuid)]
referenceReferences (CrossReference' reference) = [EntityReference "targetUuid" AnyEntityKind [reference.targetUuid]]
referenceReferences (URLReference' _) = []
