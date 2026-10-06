module Shared.Database.Migration.Development.Telemetry.TelemetryEventSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Constant.Component
import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/TelemetryEvent) drop tables"
  let sql = "DROP TABLE IF EXISTS telemetry_event CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/TelemetryEvent) create table"
  let sql =
        "CREATE TABLE telemetry_event \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    component   varchar     NOT NULL, \
        \    action      varchar     NOT NULL, \
        \    entity      varchar     NOT NULL, \
        \    body        jsonb       NOT NULL, \
        \    created_by  uuid, \
        \    tenant_uuid uuid        NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT telemetry_event_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT telemetry_event_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT telemetry_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
