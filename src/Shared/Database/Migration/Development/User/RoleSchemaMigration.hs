module Shared.Database.Migration.Development.User.RoleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Role) drop tables"
  let sql = "DROP TABLE IF EXISTS role CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Role) create table"
  let sql =
        "CREATE TABLE role \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    name         varchar     NOT NULL, \
        \    permissions  varchar[]   NOT NULL, \
        \    is_admin     boolean     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT role_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT role_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
