module Shared.Model.KnowledgeModel.KnowledgeModelEntityKind where

import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity

data EntityKind
  = KnowledgeModelEntityKind
  | ChapterEntityKind
  | QuestionEntityKind
  | OptionsQuestionEntityKind
  | MultiChoiceQuestionEntityKind
  | ListQuestionEntityKind
  | AnswerEntityKind
  | ChoiceEntityKind
  | ExpertEntityKind
  | ReferenceEntityKind
  | TagEntityKind
  | IntegrationEntityKind
  | MetricEntityKind
  | PhaseEntityKind
  | ResourceCollectionEntityKind
  | ResourcePageEntityKind
  | AnyEntityKind
  deriving (Show, Eq, Enum, Bounded)

isOfEntityKind :: KnowledgeModel -> EntityKind -> U.UUID -> Bool
isOfEntityKind km kind = maybe False (matchesEntityKind kind) . findEntity km

matchesEntityKind :: EntityKind -> KnowledgeModelEntity -> Bool
matchesEntityKind AnyEntityKind _ = True
matchesEntityKind kind (QuestionEntity question) = kind `elem` [QuestionEntityKind, toQuestionEntityKind question]
matchesEntityKind kind entity = kind == entityKind entity

entityKind :: KnowledgeModelEntity -> EntityKind
entityKind entity =
  case entity of
    KnowledgeModelEntity -> KnowledgeModelEntityKind
    ChapterEntity _ -> ChapterEntityKind
    QuestionEntity _ -> QuestionEntityKind
    AnswerEntity _ -> AnswerEntityKind
    ChoiceEntity _ -> ChoiceEntityKind
    ExpertEntity _ -> ExpertEntityKind
    ReferenceEntity _ -> ReferenceEntityKind
    IntegrationEntity _ -> IntegrationEntityKind
    TagEntity _ -> TagEntityKind
    MetricEntity _ -> MetricEntityKind
    PhaseEntity _ -> PhaseEntityKind
    ResourceCollectionEntity _ -> ResourceCollectionEntityKind
    ResourcePageEntity _ -> ResourcePageEntityKind

toQuestionEntityKind :: Question -> EntityKind
toQuestionEntityKind (OptionsQuestion' _) = OptionsQuestionEntityKind
toQuestionEntityKind (MultiChoiceQuestion' _) = MultiChoiceQuestionEntityKind
toQuestionEntityKind (ListQuestion' _) = ListQuestionEntityKind
toQuestionEntityKind _ = QuestionEntityKind

entityKindName :: EntityKind -> String
entityKindName kind =
  case kind of
    KnowledgeModelEntityKind -> "knowledge model"
    ChapterEntityKind -> "chapter"
    QuestionEntityKind -> "question"
    OptionsQuestionEntityKind -> "options question"
    MultiChoiceQuestionEntityKind -> "multi-choice question"
    ListQuestionEntityKind -> "list question"
    AnswerEntityKind -> "answer"
    ChoiceEntityKind -> "choice"
    ExpertEntityKind -> "expert"
    ReferenceEntityKind -> "reference"
    TagEntityKind -> "tag"
    IntegrationEntityKind -> "integration"
    MetricEntityKind -> "metric"
    PhaseEntityKind -> "phase"
    ResourceCollectionEntityKind -> "resource collection"
    ResourcePageEntityKind -> "resource page"
    AnyEntityKind -> "entity"

entityKindId :: EntityKind -> String
entityKindId kind =
  case kind of
    KnowledgeModelEntityKind -> "knowledgeModel"
    ChapterEntityKind -> "chapter"
    QuestionEntityKind -> "question"
    OptionsQuestionEntityKind -> "optionsQuestion"
    MultiChoiceQuestionEntityKind -> "multiChoiceQuestion"
    ListQuestionEntityKind -> "listQuestion"
    AnswerEntityKind -> "answer"
    ChoiceEntityKind -> "choice"
    ExpertEntityKind -> "expert"
    ReferenceEntityKind -> "reference"
    TagEntityKind -> "tag"
    IntegrationEntityKind -> "integration"
    MetricEntityKind -> "metric"
    PhaseEntityKind -> "phase"
    ResourceCollectionEntityKind -> "resourceCollection"
    ResourcePageEntityKind -> "resourcePage"
    AnyEntityKind -> "entity"

entityKindsName :: [EntityKind] -> String
entityKindsName kinds =
  case fmap entityKindName kinds of
    [] -> ""
    [name] -> name
    names -> L.intercalate ", " (init names) ++ " or " ++ last names

isOfAnyEntityKind :: KnowledgeModel -> [EntityKind] -> U.UUID -> Bool
isOfAnyEntityKind km kinds uuid = any (\kind -> isOfEntityKind km kind uuid) kinds
