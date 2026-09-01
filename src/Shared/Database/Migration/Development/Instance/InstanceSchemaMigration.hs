module Shared.Database.Migration.Development.Instance.InstanceSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Instance) drop tables"
  let sql = "DROP TABLE IF EXISTS instance_config_mail;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/InstanceConfigMail) create tables"
  let sql =
        "CREATE TABLE instance_config_mail \
        \( \
        \    uuid              uuid    NOT NULL, \
        \    enabled           boolean NOT NULL, \
        \    sender_name       text, \
        \    sender_email      text    NOT NULL, \
        \    smtp_host             text, \
        \    smtp_port             integer, \
        \    smtp_security         text, \
        \    smtp_username         text, \
        \    smtp_password         text, \
        \    rate_limit_window     integer, \
        \    rate_limit_count      integer, \
        \    timeout               integer, \
        \    provider              text    NOT NULL DEFAULT 'smtp', \
        \    aws_access_key_id     text, \
        \    aws_secret_access_key text, \
        \    aws_region            text, \
        \    CONSTRAINT instance_config_mail_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action
