module Shared.Database.Migration.Development.Registry.RegistryMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.Registry.RegistryLocaleDAO
import Shared.Database.DAO.Registry.RegistryMailTemplateLocaleDAO
import Shared.Database.DAO.Registry.RegistryTemplateDAO
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import Shared.Database.Migration.Development.Registry.Data.RegistryTemplates
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Registry/Package) started"
  deleteRegistryPackages
  deleteRegistryTemplates
  deleteRegistryLocales
  deleteRegistryMailTemplateLocales
  insertRegistryPackage globalRegistryPackage
  insertRegistryPackage nlRegistryPackage
  insertRegistryTemplate commonWizardRegistryTemplate
  insertRegistryLocale deRegistryLocale
  insertRegistryLocale nlRegistryLocale
  insertRegistryMailTemplateLocale deRegistryMailTemplateLocale
  logInfo _CMP_MIGRATION "(Registry/Package) ended"
