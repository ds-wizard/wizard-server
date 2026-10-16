module Shared.Database.Migration.Development.Websocket.WebsocketSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Constant.Component
import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Websocket) drop tables"
  let sql = "DROP TABLE IF EXISTS websocket_connection CASCADE; DROP TABLE IF EXISTS websocket_outbox CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  createWebsocketConnectionTable
  createWebsocketOutboxTable

createWebsocketConnectionTable :: RequestContextC s sc m => m Int64
createWebsocketConnectionTable = do
  logInfo _CMP_MIGRATION "(Table/WebsocketConnection) create table"
  let sql =
        "CREATE TABLE websocket_connection \
        \( \
        \    uuid             uuid        NOT NULL, \
        \    node_uuid        uuid        NOT NULL, \
        \    tenant_uuid      uuid        NOT NULL, \
        \    subscription     varchar     NOT NULL, \
        \    entity_uuid      uuid        NOT NULL, \
        \    user_uuid        uuid, \
        \    websocket_perm   varchar     NOT NULL, \
        \    user_group_uuids uuid[]      NOT NULL, \
        \    online_user      jsonb       NOT NULL, \
        \    created_at       timestamptz NOT NULL, \
        \    heartbeat_at     timestamptz NOT NULL, \
        \    CONSTRAINT websocket_connection_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT websocket_connection_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT websocket_connection_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX websocket_connection_entity_idx ON websocket_connection (tenant_uuid, subscription, entity_uuid); \
        \CREATE INDEX websocket_connection_node_uuid_idx ON websocket_connection (node_uuid); \
        \CREATE INDEX websocket_connection_heartbeat_at_idx ON websocket_connection (heartbeat_at);"
  let action conn = execute_ conn sql
  runDB action

createWebsocketOutboxTable :: RequestContextC s sc m => m Int64
createWebsocketOutboxTable = do
  logInfo _CMP_MIGRATION "(Table/WebsocketOutbox) create table"
  let sql =
        "CREATE TABLE websocket_outbox \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    payload     jsonb       NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT websocket_outbox_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT websocket_outbox_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX websocket_outbox_created_at_idx ON websocket_outbox (created_at);"
  let action conn = execute_ conn sql
  runDB action
