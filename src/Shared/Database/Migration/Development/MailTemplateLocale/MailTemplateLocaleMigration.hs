module Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration where

import Control.Monad (void)

import Shared.Constant.Component
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Model.Context.WizardRequestContext
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.S3.MailTemplateLocale.MailTemplateLocaleS3
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(MailTemplateLocale/MailTemplateLocale) started"
  void deleteMailTemplateLocales
  void $ insertMailTemplateLocale mailTemplateLocaleDe
  logInfo _CMP_MIGRATION "(MailTemplateLocale/MailTemplateLocale) ended"

runS3Migration :: WizardRequestContextC s m => m ()
runS3Migration = void $ putMailTemplateLocalePo mailTemplateLocaleDe.uuid mailTemplateLocaleDePo
