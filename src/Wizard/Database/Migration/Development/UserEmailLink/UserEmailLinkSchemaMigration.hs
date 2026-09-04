module Wizard.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/UserEmailLink) drop tables"
  let sql = "DROP TABLE IF EXISTS w_user_email_link CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/UserEmailLink) create table"
  let sql =
        "CREATE TABLE w_user_email_link \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    identity    uuid        NOT NULL, \
        \    type        varchar     NOT NULL, \
        \    hash        varchar     NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    CONSTRAINT w_user_email_link_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT w_user_email_link_identity_fk FOREIGN KEY (identity) REFERENCES w_user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_user_email_link_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE UNIQUE INDEX w_user_email_link_hash_uindex ON w_user_email_link (hash);"
  let action conn = execute_ conn sql
  runDB action
