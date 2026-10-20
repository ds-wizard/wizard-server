module Shared.Service.KnowledgeModel.Editor.Event.EditorEventValueValidation where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Localization.LocaleRecord
import Shared.Util.Reflection (constructorName)
import Shared.Util.ValueFormat

validateEventValues :: KnowledgeModel -> KnowledgeModel -> KnowledgeModelEvent -> [(String, LocaleRecord)]
validateEventValues km nextKm event =
  filter (`notElem` getValueErrors km event.entityUuid) (getValueErrors nextKm event.entityUuid)

getValueErrors :: KnowledgeModel -> U.UUID -> [(String, LocaleRecord)]
getValueErrors km = maybe [] entityValueErrors . findEntity km

entityValueErrors :: KnowledgeModelEntity -> [(String, LocaleRecord)]
entityValueErrors entity =
  case entity of
    KnowledgeModelEntity -> []
    ChapterEntity chapter -> requireText "title" chapter.title
    QuestionEntity question -> questionValueErrors question
    AnswerEntity answer -> answerValueErrors answer
    ChoiceEntity choice -> requireText "label" choice.aLabel
    ExpertEntity expert -> expertValueErrors expert
    ReferenceEntity reference -> referenceValueErrors reference
    IntegrationEntity integration -> requireText "name" (integrationName integration)
    TagEntity tag -> requireText "name" tag.name
    MetricEntity metric -> requireText "title" metric.title
    PhaseEntity phase -> requireText "title" phase.title
    ResourceCollectionEntity collection -> requireText "title" collection.title
    ResourcePageEntity page -> requireText "title" page.title

questionValueErrors :: Question -> [(String, LocaleRecord)]
questionValueErrors question = requireText "title" (getTitle question) ++ typeSpecific question
  where
    typeSpecific (ValueQuestion' q) = fmap ((,) "content.validations") (validationErrors q.valueType q.validations)
    typeSpecific _ = []

answerValueErrors :: Answer -> [(String, LocaleRecord)]
answerValueErrors answer =
  requireText "label" answer.aLabel
    ++ [ ("content.metricMeasures", _ERROR_VALIDATION__KM_EVENT_OUT_OF_UNIT_RANGE (show value))
       | measure <- answer.metricMeasures
       , value <- [measure.measure, measure.weight]
       , value < 0 || value > 1
       ]

expertValueErrors :: Expert -> [(String, LocaleRecord)]
expertValueErrors expert =
  requireText "name" expert.name
    ++ [("content.email", _ERROR_VALIDATION__KM_EVENT_INVALID_EMAIL expert.email) | not (isEmail expert.email)]

referenceValueErrors :: Reference -> [(String, LocaleRecord)]
referenceValueErrors (URLReference' reference) =
  [("content.url", _ERROR_VALIDATION__KM_EVENT_INVALID_URL reference.url) | not (isUrl reference.url)]
referenceValueErrors _ = []

requireText :: String -> String -> [(String, LocaleRecord)]
requireText field value = [("content." ++ field, _ERROR_VALIDATION__KM_EVENT_EMPTY_TEXT) | all isSpace value]

validationErrors :: QuestionValueType -> [QuestionValidation] -> [LocaleRecord]
validationErrors valueType validations = concatMap (validationError valueType) validations ++ rangeErrors validations

validationError :: QuestionValueType -> QuestionValidation -> [LocaleRecord]
validationError valueType validation
  | valueType `notElem` applicableValueTypes validation =
      [_ERROR_VALIDATION__KM_EVENT_VALIDATION_VALUE_TYPE (constructorName validation) (show valueType)]
validationError _ (RegexQuestionValidation regex) = [_ERROR_VALIDATION__KM_EVENT_INVALID_REGEX regex | not (isRegex regex)]
validationError _ _ = []

applicableValueTypes :: QuestionValidation -> [QuestionValueType]
applicableValueTypes validation =
  case validation of
    MinLengthQuestionValidation _ -> textValueTypes
    MaxLengthQuestionValidation _ -> textValueTypes
    RegexQuestionValidation _ -> textValueTypes
    OrcidQuestionValidation -> [StringQuestionValueType, TextQuestionValueType, UrlQuestionValueType]
    DoiQuestionValidation -> [StringQuestionValueType, TextQuestionValueType, UrlQuestionValueType]
    MinNumberQuestionValidation _ -> [NumberQuestionValueType]
    MaxNumberQuestionValidation _ -> [NumberQuestionValueType]
    FromDateQuestionValidation _ -> [DateQuestionValueType]
    ToDateQuestionValidation _ -> [DateQuestionValueType]
    FromDateTimeQuestionValidation _ -> [DateTimeQuestionValueType]
    ToDateTimeQuestionValidation _ -> [DateTimeQuestionValueType]
    FromTimeQuestionValidation _ -> [TimeQuestionValueType]
    ToTimeQuestionValidation _ -> [TimeQuestionValueType]
    DomainQuestionValidation _ -> [EmailQuestionValueType, UrlQuestionValueType]
  where
    textValueTypes = [StringQuestionValueType, TextQuestionValueType, EmailQuestionValueType, UrlQuestionValueType]

rangeErrors :: [QuestionValidation] -> [LocaleRecord]
rangeErrors validations =
  concat
    [ invertedRanges Just show [limit | MinLengthQuestionValidation limit <- validations] [limit | MaxLengthQuestionValidation limit <- validations]
    , invertedRanges Just show [limit | MinNumberQuestionValidation limit <- validations] [limit | MaxNumberQuestionValidation limit <- validations]
    , invertedRanges readDate id [limit | FromDateQuestionValidation limit <- validations] [limit | ToDateQuestionValidation limit <- validations]
    , invertedRanges readDateTime id [limit | FromDateTimeQuestionValidation limit <- validations] [limit | ToDateTimeQuestionValidation limit <- validations]
    , invertedRanges readTime id [limit | FromTimeQuestionValidation limit <- validations] [limit | ToTimeQuestionValidation limit <- validations]
    ]

invertedRanges :: Ord b => (a -> Maybe b) -> (a -> String) -> [a] -> [a] -> [LocaleRecord]
invertedRanges parse display lowerLimits upperLimits =
  [ _ERROR_VALIDATION__KM_EVENT_INVERTED_RANGE (display lowerLimit) (display upperLimit)
  | lowerLimit <- lowerLimits
  , upperLimit <- upperLimits
  , fromMaybe False ((>) <$> parse lowerLimit <*> parse upperLimit)
  ]
