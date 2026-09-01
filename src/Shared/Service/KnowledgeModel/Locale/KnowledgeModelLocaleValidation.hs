module Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Aeson (Value, eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error

validateJsonContent :: WizardRequestContextC s m => BS.ByteString -> m ()
validateJsonContent jsonContent =
  case eitherDecodeStrict jsonContent :: Either String Value of
    Left reason -> throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_INVALID_JSON reason
    Right _ -> return ()

validateCodeUniqueness :: WizardRequestContextC s m => U.UUID -> String -> m ()
validateCodeUniqueness pkgUuid code = do
  mLocale <- findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code
  when (isJust mLocale) (throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS code)
