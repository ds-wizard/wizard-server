module Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/MailTemplateLocale) drop table"
  let sql = "DROP TABLE IF EXISTS mail_template_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: WizardRequestContextC s m => m Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/MailTemplateLocale) drop triggers"
  let sql = "DROP TRIGGER IF EXISTS trg_mail_template_locale_after_delete_s3 ON mail_template_locale;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/MailTemplateLocale) create table"
  let sql =
        "CREATE TABLE mail_template_locale\
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
        \    CONSTRAINT mail_template_locale_pk PRIMARY KEY (uuid),\
        \    CONSTRAINT mail_template_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT mail_template_locale_code_unique UNIQUE (tenant_uuid, code), \
        \    CONSTRAINT mail_template_locale_default_enabled_check CHECK (NOT default_locale OR enabled) \
        \); \
        \CREATE UNIQUE INDEX mail_template_locale_default_locale_uindex ON mail_template_locale (tenant_uuid) WHERE default_locale;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: WizardRequestContextC s m => m Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/MailTemplateLocale) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trg_mail_template_locale_after_delete_s3 \
        \    AFTER DELETE \
        \    ON mail_template_locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('mail_template_locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  runDB action
