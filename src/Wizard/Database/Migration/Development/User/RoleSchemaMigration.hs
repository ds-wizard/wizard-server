module Wizard.Database.Migration.Development.User.RoleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Role) drop tables"
  let sql = "DROP TABLE IF EXISTS w_role CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Role) create table"
  let sql =
        "CREATE TABLE w_role \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    name         varchar     NOT NULL, \
        \    permissions  varchar[]   NOT NULL, \
        \    is_admin     boolean     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT w_role_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT w_role_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES w_tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
