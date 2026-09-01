module Shared.Service.Project.ProjectValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_, traverse_)
import qualified Data.Map.Strict as M
import Text.Regex.TDFA

import Shared.Api.Resource.Project.ProjectSettingsChangeDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleValidation

validateProjectSettingsChangeDTO :: WizardRequestContextC s m => ProjectSettingsChangeDTO -> m ()
validateProjectSettingsChangeDTO reqDto = do
  validateProjectTags reqDto.projectTags
  forM_ reqDto.documentTemplateUuid $ \dtUuid -> do
    tml <- findDocumentTemplateByUuid dtUuid
    validateLanguageAvailability tml reqDto.documentTemplateLanguage

validateProjectTags :: WizardRequestContextC s m => [String] -> m ()
validateProjectTags = traverse_ validateProjectTag

validateProjectTag :: WizardRequestContextC s m => String -> m ()
validateProjectTag tag = forM_ (isValidProjectTag tag) throwError

isValidProjectTag :: String -> Maybe AppError
isValidProjectTag tag =
  if tag =~ "^[^,]+$"
    then Nothing
    else Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tag])
