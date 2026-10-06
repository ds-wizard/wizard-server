module Shared.Service.Coordinate.CoordinateValidation where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Regex

import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Error.Error

validateIdentifierFormat :: MonadError AppError m => String -> String -> m ()
validateIdentifierFormat fieldName value = forM_ (isValidIdentifierFormat fieldName value) throwError

isValidIdentifierFormat :: String -> String -> Maybe AppError
isValidIdentifierFormat fieldName value =
  if isJust $ matchRegex validationRegex value
    then Nothing
    else Just $ ValidationError [] (M.singleton fieldName [_ERROR_VALIDATION__INVALID_COORDINATE_PART_FORMAT fieldName value])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9_.-]+$"

validateVersionFormat :: MonadError AppError m => Bool -> String -> m ()
validateVersionFormat allowLatest version = forM_ (isValidVersionFormat allowLatest version) throwError

isValidVersionFormat :: Bool -> String -> Maybe AppError
isValidVersionFormat allowLatest version
  | allowLatest && version == "latest" = Nothing
  | isJust $ matchRegex validationRegex version = Nothing
  | otherwise = Just . UserError $ _ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT
  where
    validationRegex = mkRegex "^[0-9]+\\.[0-9]+\\.[0-9]+$"
