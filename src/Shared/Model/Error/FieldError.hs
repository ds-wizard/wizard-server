module Shared.Model.Error.FieldError where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as M

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Localization.LocaleRecord

indexedField :: String -> Int -> String -> String
indexedField list index field = list ++ "[" ++ show index ++ "]." ++ field

keyedField :: String -> String -> String -> String
keyedField list key field = list ++ "[" ++ show key ++ "]." ++ field

fieldValidationError :: [LocaleRecord] -> [(String, LocaleRecord)] -> AppError
fieldValidationError messages = ValidationError messages . M.fromListWith (flip (++)) . fmap (fmap pure)

throwFieldErrors :: MonadError AppError m => [(String, LocaleRecord)] -> m ()
throwFieldErrors errors = unless (null errors) (throwError (fieldValidationError [] errors))

validateBatchLimit :: MonadError AppError m => Int -> [a] -> m ()
validateBatchLimit limit items =
  when (length items > limit) (throwFieldErrors [("events", _ERROR_VALIDATION__EVENT_BATCH_SIZE limit)])
