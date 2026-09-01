module Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigPrivacyAndSupport ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

entityName = "config_privacy_and_support"

findTenantConfigPrivacyAndSupport :: WizardRequestContextC s m => m TenantConfigPrivacyAndSupport
findTenantConfigPrivacyAndSupport = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigPrivacyAndSupportByUuid tenantUuid

findTenantConfigPrivacyAndSupportByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigPrivacyAndSupport
findTenantConfigPrivacyAndSupportByUuid uuid = createFindEntityByFn entityName [("tenant_uuid", U.toString uuid)]

insertTenantConfigPrivacyAndSupport :: WizardRequestContextC s m => TenantConfigPrivacyAndSupport -> m Int64
insertTenantConfigPrivacyAndSupport = createInsertFn entityName

updateTenantConfigPrivacyAndSupport :: WizardRequestContextC s m => TenantConfigPrivacyAndSupport -> m Int64
updateTenantConfigPrivacyAndSupport config = do
  let sql = fromString "UPDATE config_privacy_and_support SET tenant_uuid = ?, privacy_url = ?, terms_of_service_url = ?, support_email = ?, support_site_name = ?, support_site_url = ?, support_site_icon = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?"
  let params = toRow config ++ [toField config.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigPrivacyAndSupports :: WizardRequestContextC s m => m Int64
deleteTenantConfigPrivacyAndSupports = createDeleteEntitiesFn entityName
