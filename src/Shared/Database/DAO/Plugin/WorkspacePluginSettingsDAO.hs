module Shared.Database.DAO.Plugin.WorkspacePluginSettingsDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Plugin.WorkspacePluginSettings ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Plugin.WorkspacePluginSettings

entityName = "workspace_plugin_settings"

findWorkspacePluginSettingsByWorkspaceUuid :: WizardRequestContextC s m => U.UUID -> m [WorkspacePluginSettings]
findWorkspacePluginSettingsByWorkspaceUuid workspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("workspace_uuid", U.toString workspaceUuid)]

findWorkspacePluginSettings' :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (Maybe WorkspacePluginSettings)
findWorkspacePluginSettings' workspaceUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("workspace_uuid", U.toString workspaceUuid), ("plugin_uuid", U.toString pluginUuid)]

insertWorkspacePluginSettings :: WizardRequestContextC s m => WorkspacePluginSettings -> m Int64
insertWorkspacePluginSettings = createInsertFn entityName

saveWorkspacePluginEnabled :: WizardRequestContextC s m => U.UUID -> U.UUID -> Bool -> m ()
saveWorkspacePluginEnabled workspaceUuid pluginUuid enabled = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "INSERT INTO workspace_plugin_settings (workspace_uuid, plugin_uuid, tenant_uuid, enabled, values, created_at, updated_at) \
          \VALUES (?, ?, ?, ?, NULL, now(), now()) \
          \ON CONFLICT ON CONSTRAINT workspace_plugin_settings_pk DO UPDATE SET enabled = EXCLUDED.enabled, updated_at = now()"
  let params = [toField workspaceUuid, toField pluginUuid, toField tenantUuid, toField enabled]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

saveWorkspacePluginValues :: WizardRequestContextC s m => U.UUID -> U.UUID -> A.Value -> m ()
saveWorkspacePluginValues workspaceUuid pluginUuid values = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "INSERT INTO workspace_plugin_settings (workspace_uuid, plugin_uuid, tenant_uuid, enabled, values, created_at, updated_at) \
          \VALUES (?, ?, ?, true, ?, now(), now()) \
          \ON CONFLICT ON CONSTRAINT workspace_plugin_settings_pk DO UPDATE SET values = EXCLUDED.values, updated_at = now()"
  let params = [toField workspaceUuid, toField pluginUuid, toField tenantUuid, toJSONField values]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

resetWorkspacePluginValues :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
resetWorkspacePluginValues workspaceUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE workspace_plugin_settings SET values = NULL, updated_at = now() WHERE tenant_uuid = ? AND workspace_uuid = ? AND plugin_uuid = ?"
  let params = [toField tenantUuid, toField workspaceUuid, toField pluginUuid]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

resetWorkspacePluginValuesByPluginUuid :: WizardRequestContextC s m => U.UUID -> m ()
resetWorkspacePluginValuesByPluginUuid pluginUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE workspace_plugin_settings SET values = NULL, updated_at = now() WHERE tenant_uuid = ? AND plugin_uuid = ? AND values IS NOT NULL"
  let params = [toField tenantUuid, toField pluginUuid]
  logQuery sql params
  void $ runDB (\conn -> execute conn sql params)

deleteWorkspacePluginSettings :: WizardRequestContextC s m => m Int64
deleteWorkspacePluginSettings = createDeleteEntitiesFn entityName
