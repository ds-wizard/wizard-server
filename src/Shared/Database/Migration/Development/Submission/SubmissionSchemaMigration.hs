module Shared.Database.Migration.Development.Submission.SubmissionSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) drop tables"
  let sql = "DROP TABLE IF EXISTS submission CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Submission) create table"
  let sql =
        "CREATE TABLE submission \
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
        \    CONSTRAINT submission_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT submission_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service (tenant_uuid, id) ON DELETE CASCADE, \
        \    CONSTRAINT submission_document_uuid_fk FOREIGN KEY (document_uuid) REFERENCES document (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT submission_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX submission_document_uuid_index ON submission (document_uuid, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action
