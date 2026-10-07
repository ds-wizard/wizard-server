module Shared.Service.Locale.Common.LocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Error.Error

validateLocaleChange :: MonadError AppError m => LocaleChangeDTO -> Bool -> m ()
validateLocaleChange reqDto isDefault = do
  when (isDefault && not reqDto.enabled) $
    throwError . UserError $
      _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE
  when (reqDto.defaultLocale && not reqDto.enabled) $
    throwError . UserError $
      _ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT
