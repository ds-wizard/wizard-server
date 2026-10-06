module Shared.Database.Migration.Development.Plugin.PluginSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Plugin) drop tables"
  let sql =
        "DROP TABLE IF EXISTS workspace_plugin_settings CASCADE; \
        \DROP TABLE IF EXISTS plugin CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createPluginTable
  createWorkspacePluginSettingsTable

createPluginTable :: WizardRequestContextC s m => m Int64
createPluginTable = do
  logInfo _CMP_MIGRATION "(Table/Plugin) create table"
  let sql =
        "CREATE TABLE plugin \
        \( \
        \    uuid                       uuid        NOT NULL, \
        \    url                        varchar     NOT NULL, \
        \    enabled                    boolean     NOT NULL, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    updated_at                 timestamptz NOT NULL, \
        \    workspace_override_allowed boolean     NOT NULL DEFAULT true, \
        \    CONSTRAINT plugin_pk PRIMARY KEY (uuid, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createWorkspacePluginSettingsTable :: WizardRequestContextC s m => m Int64
createWorkspacePluginSettingsTable = do
  logInfo _CMP_MIGRATION "(Table/WorkspacePluginSettings) create table"
  let sql =
        "CREATE TABLE workspace_plugin_settings \
        \( \
        \    workspace_uuid uuid        NOT NULL, \
        \    plugin_uuid    uuid        NOT NULL, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    enabled        boolean     NOT NULL DEFAULT true, \
        \    values         jsonb, \
        \    created_at     timestamptz NOT NULL, \
        \    updated_at     timestamptz NOT NULL, \
        \    CONSTRAINT workspace_plugin_settings_pk PRIMARY KEY (workspace_uuid, plugin_uuid), \
        \    CONSTRAINT workspace_plugin_settings_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
