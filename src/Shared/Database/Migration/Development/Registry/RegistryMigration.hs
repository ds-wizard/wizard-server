module Shared.Database.Migration.Development.Registry.RegistryMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Shared.Database.DAO.Registry.RegistryOrganizationDAO
import Shared.Database.DAO.Registry.RegistryTemplateDAO
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import Shared.Database.Migration.Development.Registry.Data.RegistryTemplates
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Registry/Package) started"
  deleteRegistryOrganizations
  deleteRegistryPackages
  deleteRegistryTemplates
  insertRegistryOrganization globalRegistryOrganization
  insertRegistryOrganization nlRegistryOrganization
  insertRegistryPackage globalRegistryPackage
  insertRegistryPackage nlRegistryPackage
  insertRegistryTemplate commonWizardRegistryTemplate
  logInfo _CMP_MIGRATION "(Registry/Package) ended"
