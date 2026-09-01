module Shared.Database.Migration.Development.Plugin.PluginSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Plugin) drop tables"
  let sql = "DROP TABLE IF EXISTS plugin CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Plugin) create table"
  let sql =
        "CREATE TABLE plugin \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    url          varchar     NOT NULL, \
        \    enabled      boolean     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT plugin_pk PRIMARY KEY (uuid, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
