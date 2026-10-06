module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation where

import Control.Monad (forM_, unless)
import Control.Monad.Except (throwError)
import Data.Maybe (maybeToList)
import qualified Data.UUID as U

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Util.Reference

validateIsVersionHigher :: WizardRequestContextC s m => String -> String -> m ()
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

validatePackageIdUniqueness :: WizardRequestContextC s m => Coordinate -> Maybe U.UUID -> m ()
validatePackageIdUniqueness pkgCoordinate mWorkspaceUuid = do
  pkgs <-
    case mWorkspaceUuid of
      Just _ -> maybeToList <$> findPackageByCoordinate' pkgCoordinate mWorkspaceUuid
      Nothing -> findPackagesFiltered [("id", pkgCoordinate.id), ("version", pkgCoordinate.version)]
  unless (null pkgs) (throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show pkgCoordinate))

validatePreviousPackageIdExistence :: WizardRequestContextC s m => Coordinate -> Coordinate -> Maybe U.UUID -> m ()
validatePreviousPackageIdExistence pkgCoordinate previousPkgCoordinate mWorkspaceUuid = do
  mPkg <- findPackageByCoordinate' previousPkgCoordinate mWorkspaceUuid
  case mPkg of
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST (show previousPkgCoordinate) (show pkgCoordinate)

validateMaybePreviousPackageIdExistence :: WizardRequestContextC s m => Coordinate -> Maybe Coordinate -> Maybe U.UUID -> m ()
validateMaybePreviousPackageIdExistence pkgCoordinate mPreviousPkgCoordinate mWorkspaceUuid =
  forM_ mPreviousPkgCoordinate (\previousPkgCoordinate -> validatePreviousPackageIdExistence pkgCoordinate previousPkgCoordinate mWorkspaceUuid)
