module Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration where

import Data.Foldable (traverse_)

import Shared.Constant.Component
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.S3.DocumentTemplate.DocumentTemplateLocaleS3
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) started"
  deleteDraftData
  deleteDocumentTemplateLocales
  deleteDocumentTemplates
  insertDocumentTemplate wizardDocumentTemplate
  insertDocumentTemplateLocale czechWizardDocumentTemplateLocale
  traverse_ insertDocumentTemplateFormat wizardDocumentTemplateFormats
  insertDocumentTemplate wizardDocumentTemplateDraft
  traverse_ insertDocumentTemplateFormat wizardDocumentTemplateDraftFormats
  _ <- insertFile fileDefaultHtml
  _ <- insertFile fileDefaultCss
  _ <- insertAsset assetLogo
  insertDocumentTemplate differentDocumentTemplate
  traverse_ insertDocumentTemplateFormat differentDocumentTemplateFormats
  _ <- insertFile differentFileHtml
  logInfo _CMP_MIGRATION "(DocumentTemplate/DocumentTemplate) ended"

runS3Migration :: WizardRequestContextC s m => m ()
runS3Migration = do
  purgeBucket
  _ <- putAsset wizardDocumentTemplate.uuid assetLogo.uuid assetLogo.contentType assetLogoContent
  _ <- putDocumentTemplateLocale czechWizardDocumentTemplateLocale.uuid translationPoFileName czechPoContent
  return ()
