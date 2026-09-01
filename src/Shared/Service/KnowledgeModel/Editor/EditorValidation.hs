module Shared.Service.KnowledgeModel.Editor.EditorValidation where

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation

validateCreateDto :: WizardRequestContextC s m => KnowledgeModelEditorCreateDTO -> m ()
validateCreateDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version
  validatePackageExistence reqDto.previousPackageUuid

validateChangeDto :: WizardRequestContextC s m => KnowledgeModelEditorChangeDTO -> m ()
validateChangeDto reqDto = do
  validateCoordinatePartFormat "kmId" reqDto.kmId
  validateVersionFormat False reqDto.version

validatePackageExistence :: WizardRequestContextC s m => Maybe U.UUID -> m ()
validatePackageExistence mPkgUuid =
  case mPkgUuid of
    Just pkgUuid -> do
      mPkg <- findPackageByUuid' pkgUuid
      case mPkg of
        Just _ -> return ()
        Nothing ->
          throwError $ ValidationError [] (M.singleton "previousPackageUuid" [_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE])
    Nothing -> return ()
