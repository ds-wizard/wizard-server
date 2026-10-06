module Shared.Service.DocumentTemplate.DocumentTemplateValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Maybe (maybeToList)
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Shared.Constant.DocumentTemplate
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation

validateNewDocumentTemplate :: WizardRequestContextC s m => DocumentTemplate -> Bool -> m ()
validateNewDocumentTemplate dt shouldValidateMetamodelVersion = do
  validateIdentifierFormat "id" dt.id
  validateDocumentTemplateIdUniqueness (createCoordinate dt) dt.workspaceUuid
  when shouldValidateMetamodelVersion (validateMetamodelVersion dt)

validateChangeDto :: WizardRequestContextC s m => U.UUID -> DocumentTemplateChangeDTO -> m ()
validateChangeDto uuid reqDto = validatePhase uuid reqDto.phase

validatePhase :: WizardRequestContextC s m => U.UUID -> DocumentTemplatePhase -> m ()
validatePhase uuid newPhase = do
  when
    (newPhase == DraftDocumentTemplatePhase)
    (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE (U.toString uuid) (show newPhase))

validateDocumentTemplateIdUniqueness :: WizardRequestContextC s m => Coordinate -> Maybe U.UUID -> m ()
validateDocumentTemplateIdUniqueness coordinate mWorkspaceUuid = do
  dts <-
    case mWorkspaceUuid of
      Just _ -> maybeToList <$> findDocumentTemplateByCoordinate' coordinate mWorkspaceUuid
      Nothing -> findDocumentTemplatesFiltered [("id", coordinate.id), ("version", coordinate.version)]
  unless (null dts) (throwError . UserError $ _ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS (show coordinate))

validateMetamodelVersion :: WizardRequestContextC s m => DocumentTemplate -> m ()
validateMetamodelVersion dt =
  when
    (isDocumentTemplateUnsupported dt.metamodelVersion)
    ( throwError . UserError $
        _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION (show . createCoordinate $ dt) (show dt.metamodelVersion) (show documentTemplateMetamodelVersion)
    )

validateFileAndAssetUniqueness :: forall s m. WizardRequestContextC s m => Maybe U.UUID -> U.UUID -> String -> m ()
validateFileAndAssetUniqueness mTemplateEntityUuid dtUuid fileName = do
  files <- findFilesByDocumentTemplateUuidAndFileName dtUuid fileName
  assets <- findAssetsByDocumentTemplateUuidAndFileName dtUuid fileName
  checkUniqueness files
  checkUniqueness assets
  where
    checkUniqueness :: HasField "uuid" entity U.UUID => [entity] -> m ()
    checkUniqueness arrays =
      case arrays of
        [] -> return ()
        [entity] ->
          if mTemplateEntityUuid == Just entity.uuid
            then return ()
            else throwError . UserError $ _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS
        _ -> throwError . UserError $ _ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS
