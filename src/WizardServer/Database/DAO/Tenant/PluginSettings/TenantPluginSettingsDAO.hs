module WizardServer.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO where

import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import WizardServer.Database.Mapping.Tenant.PluginSettings.TenantPluginSettings ()
import WizardServer.Model.Tenant.PluginSettings.TenantPluginSettings

entityName = "tenant_plugin_settings"

findTenantPluginSettingValues :: WizardRequestContextC s m => U.UUID -> m (M.Map U.UUID A.Value)
findTenantPluginSettingValues tenantUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT plugin_uuid, values \
          \FROM tenant_plugin_settings \
          \WHERE tenant_uuid = ?"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

findTenantPluginSettingsByPluginUuid :: WizardRequestContextC s m => U.UUID -> m TenantPluginSettings
findTenantPluginSettingsByPluginUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("plugin_uuid", U.toString pluginUuid)]

findTenantPluginSettingsByPluginUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe TenantPluginSettings)
findTenantPluginSettingsByPluginUuid' pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("plugin_uuid", U.toString pluginUuid)]

insertTenantPluginSettings :: WizardRequestContextC s m => TenantPluginSettings -> m Int64
insertTenantPluginSettings = createInsertFn entityName

updateTenantPluginSettings :: WizardRequestContextC s m => TenantPluginSettings -> m Int64
updateTenantPluginSettings pluginSettings = do
  let sql =
        fromString
          "UPDATE tenant_plugin_settings SET tenant_uuid = ?,  plugin_uuid = ?,  values = ?,  created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND plugin_uuid = ?"
  let params = toRow pluginSettings ++ [toField pluginSettings.tenantUuid, toField pluginSettings.pluginUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
