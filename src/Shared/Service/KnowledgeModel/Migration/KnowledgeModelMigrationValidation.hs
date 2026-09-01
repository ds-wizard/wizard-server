module Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationValidation where

import Control.Monad.Except (catchError, throwError)
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation

validateMigrationUniqueness :: WizardRequestContextC s m => U.UUID -> m ()
validateMigrationUniqueness bUuid = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' bUuid
  case mMs of
    Nothing -> return ()
    Just _ -> throwError . UserError $ _ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS

validateIfTargetPackageVersionIsHigher :: WizardRequestContextC s m => Coordinate -> Coordinate -> m ()
validateIfTargetPackageVersionIsHigher forkOfPackageId targetPackage =
  catchError
    (validateIsVersionHigher targetPackage.version forkOfPackageId.version)
    (\_ -> throwError . UserError $ _ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER)
