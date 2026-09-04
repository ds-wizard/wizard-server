module Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) drop tables"
  let sql = "DROP TABLE IF EXISTS w_persistent_command CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) create table"
  let sql =
        "CREATE TABLE w_persistent_command \
        \( \
        \    uuid               uuid        not null, \
        \    state              varchar     not null, \
        \    component          varchar     not null, \
        \    function           varchar     not null, \
        \    body               varchar     not null, \
        \    last_error_message varchar, \
        \    attempts           int         not null, \
        \    max_attempts       int         not null, \
        \    tenant_uuid        uuid        not null, \
        \    created_by         uuid, \
        \    created_at         timestamptz not null, \
        \    updated_at         timestamptz not null, \
        \    internal           bool        not null, \
        \    destination        varchar, \
        \    last_trace_uuid    uuid, \
        \    CONSTRAINT w_persistent_command_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT w_persistent_command_created_by_fk FOREIGN KEY (created_by) REFERENCES w_user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_persistent_command_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
