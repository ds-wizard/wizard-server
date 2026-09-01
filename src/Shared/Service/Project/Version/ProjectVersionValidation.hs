module Shared.Service.Project.Version.ProjectVersionValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error

validateProjectVersionCreate :: WizardRequestContextC s m => U.UUID -> ProjectVersionChangeDTO -> m ()
validateProjectVersionCreate projectUuid reqDto = do
  validateProjectVersionEventExistence reqDto
  validateProjectVersionUniqueness projectUuid reqDto

validateProjectVersionUpdate :: WizardRequestContextC s m => ProjectVersionChangeDTO -> m ()
validateProjectVersionUpdate = validateProjectVersionEventExistence

validateProjectVersionUniqueness :: WizardRequestContextC s m => U.UUID -> ProjectVersionChangeDTO -> m ()
validateProjectVersionUniqueness projectUuid reqDto = do
  mProjectVersion <- findProjectVersionByEventUuid' projectUuid reqDto.eventUuid
  when
    (isJust mProjectVersion)
    (throwError . UserError $ _ERROR_SERVICE_PROJECT_VERSION__VERSION_UNIQUENESS (U.toString reqDto.eventUuid))

validateProjectVersionEventExistence :: WizardRequestContextC s m => ProjectVersionChangeDTO -> m ()
validateProjectVersionEventExistence reqDto =
  findProjectEventByUuid' reqDto.eventUuid >>= \case
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PROJECT_VERSION__NON_EXISTENT_EVENT_UUID (U.toString reqDto.eventUuid)
