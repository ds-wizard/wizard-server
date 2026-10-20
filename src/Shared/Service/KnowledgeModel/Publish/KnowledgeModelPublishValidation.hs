module Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation

validateMigrationExistence editorUuid = do
  mMs <- findKnowledgeModelMigrationByEditorUuid' editorUuid
  when (isJust mMs) (throwError . UserError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__KM_MIGRATION_EXISTS)

validateNewPackageVersion pkgVersion kmEditor = do
  validateVersionFormat False pkgVersion
  mPkg <- findLatestPackageById' kmEditor.id Nothing (Just kmEditor.workspaceUuid)
  case mPkg of
    Just pkg -> validateIsVersionHigher pkgVersion pkg.version
    Nothing -> return ()

validateMigrationCompleted :: WizardRequestContextC s m => KnowledgeModelMigration -> m ()
validateMigrationCompleted migration =
  unless (migration.state == CompletedKnowledgeModelMigrationState) (throwError . UserError $ _ERROR_SERVICE_MCP__MIGRATION_NOT_COMPLETED)

validateReusableLocales :: WizardRequestContextC s m => [KnowledgeModelLocale] -> Maybe [U.UUID] -> m ()
validateReusableLocales locales mLocaleUuids =
  unless (all (`elem` fmap (.uuid) locales) (fromMaybe [] mLocaleUuids)) (throwError . UserError $ _ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE)
