module Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.DocumentTemplate.DocumentTemplateValidation (validateDocumentTemplateIdUniqueness)

validateChangeDto :: WizardRequestContextC s m => DocumentTemplateDraftChangeDTO -> DocumentTemplate -> m ()
validateChangeDto reqDto dt = do
  validateIdentifierFormat "id" reqDto.id
  let newCoordinate = Coordinate reqDto.id reqDto.version
  when
    (reqDto.id /= dt.id || reqDto.version /= dt.version)
    (validateDocumentTemplateIdUniqueness newCoordinate dt.workspaceUuid)
  validatePhase dt.uuid reqDto.phase

validatePhase :: WizardRequestContextC s m => U.UUID -> DocumentTemplatePhase -> m ()
validatePhase dtUuid newPhase = do
  when
    (newPhase == DeprecatedDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE (U.toString dtUuid) (show newPhase))
