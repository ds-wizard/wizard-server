module Shared.Database.Migration.Development.AuditLog.AuditLogSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Constant.Component
import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/AuditLog) drop table"
  let sql = "DROP TABLE IF EXISTS audit_log CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/AuditLog) create table"
  let sql =
        "CREATE TABLE audit_log \
        \( \
        \    uuid                  uuid        NOT NULL, \
        \    component             varchar     NOT NULL, \
        \    action                varchar     NOT NULL, \
        \    action_data           jsonb       NOT NULL, \
        \    workspace_uuid        uuid, \
        \    workspace_name        varchar, \
        \    created_by_uuid       uuid, \
        \    created_by_first_name varchar, \
        \    created_by_last_name  varchar, \
        \    created_by_email      varchar, \
        \    created_by_image_url  varchar, \
        \    source_type           varchar     NOT NULL, \
        \    source_uuid           uuid, \
        \    tenant_uuid           uuid        NOT NULL, \
        \    created_at            timestamptz NOT NULL, \
        \    CONSTRAINT audit_log_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT audit_log_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX audit_log_tenant_uuid_created_at_idx ON audit_log (tenant_uuid, created_at DESC); \
        \CREATE INDEX audit_log_tenant_uuid_workspace_uuid_created_at_idx ON audit_log (tenant_uuid, workspace_uuid, created_at DESC);"
  let action conn = execute_ conn sql
  runDB action
