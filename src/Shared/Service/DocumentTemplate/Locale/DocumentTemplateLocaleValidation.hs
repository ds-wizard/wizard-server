module Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust, isNothing)
import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error

validateCodeUniqueness :: WizardRequestContextC s m => U.UUID -> String -> m ()
validateCodeUniqueness dtUuid code = do
  mLocale <- findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' dtUuid code
  when (isJust mLocale) (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_CODE_UNIQUENESS code)

validateLanguageAvailability :: WizardRequestContextC s m => DocumentTemplate -> Maybe String -> m ()
validateLanguageAvailability _ Nothing = return ()
validateLanguageAvailability dt (Just language)
  | language == dt.language = return ()
  | otherwise = do
      mLocale <- findDocumentTemplateLocaleByDocumentTemplateUuidAndCode' dt.uuid language
      when (isNothing mLocale) (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_NOT_AVAILABLE language)
