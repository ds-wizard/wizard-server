module Shared.Service.User.Profile.UserProfileValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale

validateLocale :: WizardRequestContextC s m => UserLocaleDTO -> m ()
validateLocale reqDto = do
  case reqDto.uuid of
    Nothing -> return ()
    Just localeUuid -> do
      mLocale <- findLocaleByUuid' localeUuid
      case mLocale of
        Just locale ->
          if locale.enabled
            then return ()
            else throwError $ ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString localeUuid)])
        Nothing -> throwError $ ValidationError [] (M.singleton "uuid" [_ERROR_VALIDATION__ABSENCE (U.toString localeUuid)])
