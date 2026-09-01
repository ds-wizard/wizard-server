module Shared.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.WizardTenantConfigDashboardAndLoginScreen ()
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Model.Tenant.Config.WizardTenantConfig

findTenantConfigDashboardAndLoginScreen :: WizardRequestContextC s m => m TenantConfigDashboardAndLoginScreen
findTenantConfigDashboardAndLoginScreen = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigDashboardAndLoginScreenByUuid tenantUuid

findTenantConfigDashboardAndLoginScreenByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigDashboardAndLoginScreen
findTenantConfigDashboardAndLoginScreenByUuid tenantUuid = do
  config <- createFindEntityByFn "config_dashboard_and_login_screen" [("tenant_uuid", U.toString tenantUuid)]
  announcements <- createFindEntitiesBySortedFn "config_dashboard_and_login_screen_announcement" [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]
  return $ config {announcements = announcements}

insertTenantConfigDashboardAndLoginScreen :: WizardRequestContextC s m => TenantConfigDashboardAndLoginScreen -> m Int64
insertTenantConfigDashboardAndLoginScreen = createInsertFn "config_dashboard_and_login_screen"

insertTenantConfigDashboardAndLoginScreenAnnouncement :: WizardRequestContextC s m => TenantConfigDashboardAndLoginScreenAnnouncement -> m Int64
insertTenantConfigDashboardAndLoginScreenAnnouncement = createInsertFn "config_dashboard_and_login_screen_announcement"

updateTenantConfigDashboardAndLoginScreen :: WizardRequestContextC s m => TenantConfigDashboardAndLoginScreen -> m Int64
updateTenantConfigDashboardAndLoginScreen config = do
  let sql =
        fromString $
          "UPDATE config_dashboard_and_login_screen SET tenant_uuid = ?, dashboard_type = ?, login_info = ?, login_info_sidebar = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
          \DELETE FROM config_dashboard_and_login_screen_announcement WHERE tenant_uuid = ?;"
            ++ concatMap (const "INSERT INTO config_dashboard_and_login_screen_announcement VALUES (?, ?, ?, ?, ?, ?, ?, ?);") config.announcements
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.announcements
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigDashboardAndLoginScreens :: WizardRequestContextC s m => m Int64
deleteTenantConfigDashboardAndLoginScreens = do
  createDeleteEntitiesFn "config_dashboard_and_login_screen_announcement"
  createDeleteEntitiesFn "config_dashboard_and_login_screen"
