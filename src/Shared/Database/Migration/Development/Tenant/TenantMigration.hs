module Shared.Database.Migration.Development.Tenant.TenantMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Database.DAO.Tenant.Module.TenantModuleDAO
import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.TenantConfigMail
import Shared.Model.Tenant.Config.TenantConfigMailDM
import Shared.Model.Tenant.Tenant
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
  seedSettings defaultTenant.uuid
  insertTenantConfigMail (defaultMail {tenantUuid = defaultTenant.uuid} :: TenantConfigMail)
  deleteTenantModules
  mapM_ insertTenantModule defaultTenantModules

runLimitMigration :: WizardRequestContextC s m => m ()
runLimitMigration = do
  deleteLimitBundles
  insertLimitBundle defaultTenantLimitBundle
  insertLimitBundle differentTenantLimitBundle
  return ()
