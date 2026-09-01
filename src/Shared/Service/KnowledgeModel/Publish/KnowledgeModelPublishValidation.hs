module Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishValidation where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation

validateMigrationExistence editorUuid = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' editorUuid
  when (isJust mMs) (throwError . UserError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__KM_MIGRATION_EXISTS)

validateNewPackageVersion pkgVersion kmEditor org = do
  validateVersionFormat False pkgVersion
  mPkg <- findLatestPackageByOrganizationIdAndKmId' org.organizationId kmEditor.kmId Nothing
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion pkg.version
    Nothing -> return ()
