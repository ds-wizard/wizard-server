module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Util.Coordinate

validateIsVersionHigher :: WizardRequestContextC s m => String -> String -> m ()
validateIsVersionHigher newVersion oldVersion =
  if compareVersion newVersion oldVersion == GT
    then return ()
    else throwError . UserError $ _ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION

validatePackageIdUniqueness :: WizardRequestContextC s m => Coordinate -> m ()
validatePackageIdUniqueness pkgCoordinate = do
  mPkg <- findPackageByCoordinate' pkgCoordinate
  case mPkg of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__PKG_ID_UNIQUENESS (show pkgCoordinate)

validatePreviousPackageIdExistence :: WizardRequestContextC s m => Coordinate -> Coordinate -> m ()
validatePreviousPackageIdExistence pkgCoordinate previousPkgCoordinate = do
  mPkg <- findPackageByCoordinate' previousPkgCoordinate
  case mPkg of
    Just _ -> return ()
    Nothing -> throwError . UserError $ _ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST (show previousPkgCoordinate) (show pkgCoordinate)

validateMaybePreviousPackageIdExistence :: WizardRequestContextC s m => Coordinate -> Maybe Coordinate -> m ()
validateMaybePreviousPackageIdExistence pkgCoordinate mPreviousPkgCoordinate =
  forM_ mPreviousPkgCoordinate (validatePreviousPackageIdExistence pkgCoordinate)
