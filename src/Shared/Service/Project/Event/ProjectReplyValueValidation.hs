module Shared.Service.Project.Event.ProjectReplyValueValidation where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)

import Shared.Localization.Messages.WizardPublic
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Localization.LocaleRecord
import Shared.Model.Project.ProjectReply
import Shared.Util.String (splitOn, trim)
import Shared.Util.ValueFormat

validateReplyValue :: Question -> ReplyValue -> Either LocaleRecord ()
validateReplyValue (ValueQuestion' question) (StringReply value) = do
  unless (isValidValue question.valueType value) (Left . _ERROR_VALIDATION__PROJECT_EVENT_VALUE_TYPE . show $ question.valueType)
  traverse_ (validateValidation value) question.validations
validateReplyValue _ _ = Right ()

validateValidation :: String -> QuestionValidation -> Either LocaleRecord ()
validateValidation value validation =
  unless (passes value validation) (Left . _ERROR_VALIDATION__PROJECT_EVENT_VALUE_VALIDATION . show $ validation)

isValidValue :: QuestionValueType -> String -> Bool
isValidValue NumberQuestionValueType = isJust . readNumber
isValidValue DateQuestionValueType = isJust . readDate
isValidValue DateTimeQuestionValueType = isJust . readDateTime
isValidValue TimeQuestionValueType = isJust . readTime
isValidValue EmailQuestionValueType = isEmail
isValidValue UrlQuestionValueType = isUrl
isValidValue ColorQuestionValueType = matchesPosixRegex "^#[0-9a-fA-F]{6}$"
isValidValue _ = const True

passes :: String -> QuestionValidation -> Bool
passes value (MinLengthQuestionValidation limit) = length value >= limit
passes value (MaxLengthQuestionValidation limit) = length value <= limit
passes value (RegexQuestionValidation regex) = matchesRegex regex value
passes value OrcidQuestionValidation = matchesPosixRegex "^(https://orcid\\.org/)?[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$" value
passes value DoiQuestionValidation = matchesPosixRegex "^(https://doi\\.org/)?10\\.[0-9]{4,9}/[^[:space:]]+$" value
passes value (MinNumberQuestionValidation limit) = maybe True (>= limit) (readNumber value)
passes value (MaxNumberQuestionValidation limit) = maybe True (<= limit) (readNumber value)
passes value (FromDateQuestionValidation limit) = compareWith readDate (>=) value limit
passes value (ToDateQuestionValidation limit) = compareWith readDate (<=) value limit
passes value (FromDateTimeQuestionValidation limit) = compareWith readDateTime (>=) value limit
passes value (ToDateTimeQuestionValidation limit) = compareWith readDateTime (<=) value limit
passes value (FromTimeQuestionValidation limit) = compareWith readTime (>=) value limit
passes value (ToTimeQuestionValidation limit) = compareWith readTime (<=) value limit
passes value (DomainQuestionValidation domains) = any (isInDomain value . fmap toLower . trim) (splitOn "," domains)

isInDomain :: String -> String -> Bool
isInDomain value domain =
  let valueDomain = fmap toLower . reverse . takeWhile (/= '@') . reverse $ value
   in valueDomain == domain || ('.' : domain) `L.isSuffixOf` valueDomain

compareWith :: (String -> Maybe a) -> (a -> a -> Bool) -> String -> String -> Bool
compareWith parse operator value limit = fromMaybe True (operator <$> parse value <*> parse limit)

readNumber :: String -> Maybe Double
readNumber = readMaybe
