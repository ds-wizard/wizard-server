module Shared.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql =
        "DROP TABLE IF EXISTS locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: WizardRequestContextC s m => m Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Locale) drop triggers"
  let sql = "DROP TRIGGER IF EXISTS trg_locale_after_delete_s3 ON locale;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "CREATE TABLE locale\
        \(\
        \    uuid           uuid        NOT NULL,\
        \    tenant_uuid    uuid        NOT NULL,\
        \    code           varchar     NOT NULL,\
        \    name           varchar     NOT NULL,\
        \    pot_version    varchar,\
        \    enabled        bool        NOT NULL,\
        \    default_locale bool        NOT NULL,\
        \    official       bool        NOT NULL DEFAULT false,\
        \    created_at     timestamptz NOT NULL,\
        \    updated_at     timestamptz NOT NULL,\
        \    CONSTRAINT locale_pk PRIMARY KEY (uuid),\
        \    CONSTRAINT locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT locale_code_unique UNIQUE (tenant_uuid, code), \
        \    CONSTRAINT locale_default_enabled_check CHECK (NOT default_locale OR enabled) \
        \); \
        \CREATE UNIQUE INDEX locale_default_locale_uindex ON locale (tenant_uuid) WHERE default_locale;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: WizardRequestContextC s m => m Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Locale) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trg_locale_after_delete_s3 \
        \    AFTER DELETE \
        \    ON locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  runDB action
