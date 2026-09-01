module Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleUtil where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS

import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Util.Gettext

extractLanguageCode :: WizardRequestContextC s m => BS.ByteString -> m String
extractLanguageCode poContent =
  case parsePoHeaderFields (BS.unpack poContent) of
    Left reason -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_INVALID_PO reason
    Right fields ->
      case getPoHeaderField "Language" fields of
        Just code -> return code
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_LOCALE_MISSING_LANGUAGE
