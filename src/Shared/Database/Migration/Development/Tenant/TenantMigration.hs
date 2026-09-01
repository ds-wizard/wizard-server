module Shared.Database.Migration.Development.Tenant.TenantMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Shared.Database.DAO.Tenant.Module.TenantModuleDAO
import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.Migration.Development.Tenant.Data.TenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantConfigs
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Tenant/Tenant) started"
  runTenantMigration
  runConfigMigration
  runLimitMigration
  logInfo _CMP_MIGRATION "(Tenant/Tenant) ended"

runTenantMigration :: WizardRequestContextC s m => m ()
runTenantMigration = do
  deleteTenants
  insertTenant defaultTenant
  insertTenant differentTenant
  return ()

runConfigMigration :: WizardRequestContextC s m => m ()
runConfigMigration = do
  deleteTenantConfigOrganizations
  insertTenantConfigOrganization defaultOrganization
  insertTenantConfigAuthentication defaultAuthenticationEncrypted
  insertTenantConfigPrivacyAndSupport defaultPrivacyAndSupport
  insertTenantConfigDashboardAndLoginScreen defaultDashboardAndLoginScreen
  insertTenantConfigDashboardAndLoginScreenAnnouncement defaultDashboardAndLoginScreenAnnouncement
  insertTenantConfigLookAndFeel defaultLookAndFeel
  insertTenantConfigLookAndFeelCustomMenuLink defaultLookAndFeelCustomLink
  insertTenantConfigRegistry defaultRegistryEncrypted
  insertTenantConfigProject defaultProjectEncrypted
  insertTenantConfigSubmission (defaultSubmission {services = []})
  insertTenantConfigFeatures defaultFeatures
  insertTenantConfigMail defaultMail
  insertTenantConfigOwl defaultOwl
  deleteTenantModules
  mapM_ insertTenantModule defaultTenantModules

runLimitMigration :: WizardRequestContextC s m => m ()
runLimitMigration = do
  deleteLimitBundles
  insertLimitBundle defaultTenantLimitBundle
  insertLimitBundle differentTenantLimitBundle
  return ()
