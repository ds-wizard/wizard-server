module Shared.Service.KnowledgeModel.Editor.Event.EditorEventStructureValidation where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind
import Shared.Model.KnowledgeModel.KnowledgeModelUtil
import Shared.Model.Localization.LocaleRecord

data EventStructure
  = AddEventStructure EntityKind [EntityKind]
  | ExistingEventStructure EntityKind
  | MoveEventStructure EntityKind [EntityKind] U.UUID

validateEventStructure :: KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateEventStructure km event =
  case toEventStructure event.content of
    AddEventStructure kind parentKinds -> validateNewEntity km kind event.entityUuid ++ validateParent km parentKinds event.parentUuid
    ExistingEventStructure kind -> validateExistingEntity km kind event
    MoveEventStructure kind targetKinds targetUuid -> validateExistingEntity km kind event ++ validateMoveTarget km targetKinds event.entityUuid targetUuid

validateNewEntity :: KnowledgeModel -> EntityKind -> U.UUID -> [(String, LocaleRecord)]
validateNewEntity km kind uuid
  | kind == KnowledgeModelEntityKind && km.uuid /= U.nil = [("entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_UNIQUENESS (U.toString km.uuid))]
  | entityExists km uuid = [("entityUuid", _ERROR_VALIDATION__KM_EVENT_ENTITY_UNIQUENESS (U.toString uuid))]
  | otherwise = []

validateParent :: KnowledgeModel -> [EntityKind] -> U.UUID -> [(String, LocaleRecord)]
validateParent km kinds parentUuid
  | null kinds || isOfAnyEntityKind km kinds parentUuid = []
  | otherwise = [("parentUuid", absenceError parentUuid kinds)]

validateExistingEntity :: KnowledgeModel -> EntityKind -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateExistingEntity km kind event
  | not (isOfEntityKind km kind event.entityUuid) = [("entityUuid", absenceError event.entityUuid [kind])]
  | parentUuid /= event.parentUuid = [("parentUuid", _ERROR_VALIDATION__KM_EVENT_PARENT_MISMATCH (U.toString event.entityUuid) (U.toString parentUuid))]
  | otherwise = []
  where
    parentUuid = M.findWithDefault U.nil event.entityUuid (makeParentMap km)

validateMoveTarget :: KnowledgeModel -> [EntityKind] -> U.UUID -> U.UUID -> [(String, LocaleRecord)]
validateMoveTarget km kinds uuid targetUuid
  | not (isOfAnyEntityKind km kinds targetUuid) = [("content.targetUuid", absenceError targetUuid kinds)]
  | uuid `elem` (targetUuid : getAncestorUuids (makeParentMap km) targetUuid) =
      [("content.targetUuid", _ERROR_VALIDATION__KM_EVENT_MOVE_CYCLE (U.toString uuid) (U.toString targetUuid))]
  | otherwise = []

absenceError :: U.UUID -> [EntityKind] -> LocaleRecord
absenceError uuid kinds = _ERROR_VALIDATION__KM_EVENT_ENTITY_ABSENCE (U.toString uuid) (entityKindsName kinds)

isExistingEntityEvent :: KnowledgeModelEventData -> Bool
isExistingEntityEvent content =
  case toEventStructure content of
    ExistingEventStructure _ -> True
    _ -> False

toEventStructure :: KnowledgeModelEventData -> EventStructure
toEventStructure content =
  case content of
    AddKnowledgeModelEvent' _ -> AddEventStructure KnowledgeModelEntityKind []
    EditKnowledgeModelEvent' _ -> ExistingEventStructure KnowledgeModelEntityKind
    AddChapterEvent' _ -> AddEventStructure ChapterEntityKind [KnowledgeModelEntityKind]
    EditChapterEvent' _ -> ExistingEventStructure ChapterEntityKind
    DeleteChapterEvent' _ -> ExistingEventStructure ChapterEntityKind
    AddQuestionEvent' _ -> AddEventStructure QuestionEntityKind questionParentKinds
    EditQuestionEvent' _ -> ExistingEventStructure QuestionEntityKind
    DeleteQuestionEvent' _ -> ExistingEventStructure QuestionEntityKind
    AddAnswerEvent' _ -> AddEventStructure AnswerEntityKind [OptionsQuestionEntityKind]
    EditAnswerEvent' _ -> ExistingEventStructure AnswerEntityKind
    DeleteAnswerEvent' _ -> ExistingEventStructure AnswerEntityKind
    AddChoiceEvent' _ -> AddEventStructure ChoiceEntityKind [MultiChoiceQuestionEntityKind]
    EditChoiceEvent' _ -> ExistingEventStructure ChoiceEntityKind
    DeleteChoiceEvent' _ -> ExistingEventStructure ChoiceEntityKind
    AddExpertEvent' _ -> AddEventStructure ExpertEntityKind [QuestionEntityKind]
    EditExpertEvent' _ -> ExistingEventStructure ExpertEntityKind
    DeleteExpertEvent' _ -> ExistingEventStructure ExpertEntityKind
    AddReferenceEvent' _ -> AddEventStructure ReferenceEntityKind [QuestionEntityKind]
    EditReferenceEvent' _ -> ExistingEventStructure ReferenceEntityKind
    DeleteReferenceEvent' _ -> ExistingEventStructure ReferenceEntityKind
    AddTagEvent' _ -> AddEventStructure TagEntityKind [KnowledgeModelEntityKind]
    EditTagEvent' _ -> ExistingEventStructure TagEntityKind
    DeleteTagEvent' _ -> ExistingEventStructure TagEntityKind
    AddIntegrationEvent' _ -> AddEventStructure IntegrationEntityKind [KnowledgeModelEntityKind]
    EditIntegrationEvent' _ -> ExistingEventStructure IntegrationEntityKind
    DeleteIntegrationEvent' _ -> ExistingEventStructure IntegrationEntityKind
    AddMetricEvent' _ -> AddEventStructure MetricEntityKind [KnowledgeModelEntityKind]
    EditMetricEvent' _ -> ExistingEventStructure MetricEntityKind
    DeleteMetricEvent' _ -> ExistingEventStructure MetricEntityKind
    AddPhaseEvent' _ -> AddEventStructure PhaseEntityKind [KnowledgeModelEntityKind]
    EditPhaseEvent' _ -> ExistingEventStructure PhaseEntityKind
    DeletePhaseEvent' _ -> ExistingEventStructure PhaseEntityKind
    AddResourceCollectionEvent' _ -> AddEventStructure ResourceCollectionEntityKind [KnowledgeModelEntityKind]
    EditResourceCollectionEvent' _ -> ExistingEventStructure ResourceCollectionEntityKind
    DeleteResourceCollectionEvent' _ -> ExistingEventStructure ResourceCollectionEntityKind
    AddResourcePageEvent' _ -> AddEventStructure ResourcePageEntityKind [ResourceCollectionEntityKind]
    EditResourcePageEvent' _ -> ExistingEventStructure ResourcePageEntityKind
    DeleteResourcePageEvent' _ -> ExistingEventStructure ResourcePageEntityKind
    MoveQuestionEvent' move -> MoveEventStructure QuestionEntityKind questionParentKinds move.targetUuid
    MoveAnswerEvent' move -> MoveEventStructure AnswerEntityKind [OptionsQuestionEntityKind] move.targetUuid
    MoveChoiceEvent' move -> MoveEventStructure ChoiceEntityKind [MultiChoiceQuestionEntityKind] move.targetUuid
    MoveExpertEvent' move -> MoveEventStructure ExpertEntityKind [QuestionEntityKind] move.targetUuid
    MoveReferenceEvent' move -> MoveEventStructure ReferenceEntityKind [QuestionEntityKind] move.targetUuid

questionParentKinds :: [EntityKind]
questionParentKinds = [ChapterEntityKind, ListQuestionEntityKind, AnswerEntityKind]
