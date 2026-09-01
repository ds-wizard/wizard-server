module Shared.Service.Locale.LocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleCreateDTO
import Shared.Constant.Locale
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Localization.Messages.Locale.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Service.Coordinate.CoordinateValidation

validateLocaleCreate :: WizardRequestContextC s m => LocaleCreateDTO -> String -> m ()
validateLocaleCreate reqDto organizationId = do
  validateCoordinatePartFormat "localeId" reqDto.localeId
  validateLocaleIdUniqueness (Coordinate organizationId reqDto.localeId reqDto.version)

validateLocaleChange :: WizardRequestContextC s m => LocaleChangeDTO -> Locale -> m ()
validateLocaleChange reqDto locale = do
  when (not locale.enabled && not reqDto.enabled && reqDto.defaultLocale) (throwError . UserError $ _ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT)
  when (locale.defaultLocale && reqDto.defaultLocale && not reqDto.enabled) (throwError . UserError $ _ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE)

validateLocaleDeletion :: WizardRequestContextC s m => Locale -> m ()
validateLocaleDeletion locale = do
  when locale.defaultLocale (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_LOCALE_DELETION)
  when (locale.organizationId == defaultLocaleOrganizationId) (throwError . UserError $ _ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETION)

validateLocaleIdUniqueness :: WizardRequestContextC s m => Coordinate -> m ()
validateLocaleIdUniqueness coordinate = do
  mLocale <- findLocaleByCoordinate' coordinate
  case mLocale of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__LCL_ID_UNIQUENESS (show coordinate)
