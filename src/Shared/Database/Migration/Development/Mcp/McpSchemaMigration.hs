module Shared.Database.Migration.Development.Mcp.McpSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Mcp) drop tables"
  let sql =
        "DROP TABLE IF EXISTS mcp_used_confirmation_token CASCADE;\
        \DROP TABLE IF EXISTS mcp_refresh_token CASCADE;\
        \DROP TABLE IF EXISTS mcp_authorization_code CASCADE;\
        \DROP TABLE IF EXISTS mcp_client CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createMcpClientTable
  createMcpAuthorizationCodeTable
  createMcpRefreshTokenTable
  createMcpUsedConfirmationTokenTable

createMcpClientTable :: WizardRequestContextC s m => m Int64
createMcpClientTable = do
  logInfo _CMP_MIGRATION "(Table/McpClient) creating table"
  let sql =
        "CREATE TABLE mcp_client \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    client_id     varchar     NOT NULL, \
        \    client_secret varchar     NOT NULL, \
        \    client_name   varchar, \
        \    redirect_uris jsonb       NOT NULL DEFAULT '[]'::jsonb, \
        \    tenant_uuid   uuid        NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    updated_at    timestamptz NOT NULL, \
        \    token_endpoint_auth_method varchar NOT NULL DEFAULT 'client_secret_post', \
        \    CONSTRAINT mcp_client_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT mcp_client_client_id_uq UNIQUE (client_id), \
        \    CONSTRAINT mcp_client_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE INDEX mcp_client_client_id_idx ON mcp_client (client_id);"
  let action conn = execute_ conn sql
  runDB action

createMcpAuthorizationCodeTable :: WizardRequestContextC s m => m Int64
createMcpAuthorizationCodeTable = do
  logInfo _CMP_MIGRATION "(Table/McpAuthorizationCode) creating table"
  let sql =
        "CREATE TABLE mcp_authorization_code \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    code         varchar     NOT NULL, \
        \    client_id    varchar     NOT NULL, \
        \    user_uuid    uuid        NOT NULL, \
        \    redirect_uri varchar     NOT NULL, \
        \    expires_at   timestamptz NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    code_challenge        varchar, \
        \    code_challenge_method varchar, \
        \    toolsets     varchar[]   NOT NULL DEFAULT ARRAY ['km', 'dt', 'projects', 'admin', 'analytics'], \
        \    CONSTRAINT mcp_authorization_code_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT mcp_authorization_code_code_uq UNIQUE (code), \
        \    CONSTRAINT mcp_authorization_code_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT mcp_authorization_code_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE INDEX mcp_authorization_code_code_idx ON mcp_authorization_code (code);"
  let action conn = execute_ conn sql
  runDB action

createMcpRefreshTokenTable :: WizardRequestContextC s m => m Int64
createMcpRefreshTokenTable = do
  logInfo _CMP_MIGRATION "(Table/McpRefreshToken) creating table"
  let sql =
        "CREATE TABLE mcp_refresh_token \
        \( \
        \    uuid                uuid        NOT NULL, \
        \    token_hash          varchar     NOT NULL, \
        \    previous_token_hash varchar, \
        \    user_token_uuid     uuid        NOT NULL, \
        \    client_id           varchar     NOT NULL, \
        \    tenant_uuid         uuid        NOT NULL, \
        \    expires_at          timestamptz NOT NULL, \
        \    created_at          timestamptz NOT NULL, \
        \    toolsets            varchar[]   NOT NULL, \
        \    CONSTRAINT mcp_refresh_token_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT mcp_refresh_token_token_hash_uq UNIQUE (token_hash), \
        \    CONSTRAINT mcp_refresh_token_previous_token_hash_uq UNIQUE (previous_token_hash), \
        \    CONSTRAINT mcp_refresh_token_user_token_uuid_fk FOREIGN KEY (user_token_uuid) REFERENCES user_token (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT mcp_refresh_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE INDEX mcp_refresh_token_user_token_uuid_idx ON mcp_refresh_token (user_token_uuid);"
  let action conn = execute_ conn sql
  runDB action

createMcpUsedConfirmationTokenTable :: WizardRequestContextC s m => m Int64
createMcpUsedConfirmationTokenTable = do
  logInfo _CMP_MIGRATION "(Table/McpUsedConfirmationToken) creating table"
  let sql =
        "CREATE TABLE mcp_used_confirmation_token \
        \( \
        \    token_hash  varchar     NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    expires_at  timestamptz NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    CONSTRAINT mcp_used_confirmation_token_pk PRIMARY KEY (token_hash), \
        \    CONSTRAINT mcp_used_confirmation_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
