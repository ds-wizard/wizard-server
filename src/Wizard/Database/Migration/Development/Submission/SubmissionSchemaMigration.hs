module Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) drop tables"
  let sql = "DROP TABLE IF EXISTS w_submission CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) create table"
  let sql =
        "CREATE TABLE w_submission \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    state         varchar     NOT NULL, \
        \    location      varchar, \
        \    returned_data varchar, \
        \    service_id    varchar     NOT NULL, \
        \    document_uuid uuid, \
        \    created_by    uuid, \
        \    created_at    timestamptz, \
        \    updated_at    timestamptz NOT NULL, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    CONSTRAINT w_submission_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT w_submission_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES w_config_submission_service (tenant_uuid, id) ON DELETE CASCADE, \
        \    CONSTRAINT w_submission_document_uuid_fk FOREIGN KEY (document_uuid) REFERENCES w_document (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_submission_created_by_fk FOREIGN KEY (created_by) REFERENCES w_user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT w_submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES w_tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX w_submission_document_uuid_index ON w_submission (document_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
