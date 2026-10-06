module Shared.Database.Migration.Development.Workspace.WorkspaceSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Workspace) drop tables"
  let sql =
        "DROP TABLE IF EXISTS workspace_membership CASCADE; \
        \DROP TABLE IF EXISTS workspace CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Workspace) create table"
  let sql =
        "CREATE TABLE workspace \
        \( \
        \    uuid        uuid        NOT NULL, \
        \    tenant_uuid uuid        NOT NULL, \
        \    name        varchar     NOT NULL, \
        \    description varchar, \
        \    created_at  timestamptz NOT NULL, \
        \    updated_at  timestamptz NOT NULL, \
        \    default_role_uuid uuid, \
        \    logo        varchar, \
        \    primary_color varchar, \
        \    CONSTRAINT workspace_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT workspace_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX workspace_tenant_uuid_index ON workspace (tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createMembershipTable :: WizardRequestContextC s m => m Int64
createMembershipTable = do
  logInfo _CMP_MIGRATION "(Table/WorkspaceMembership) create table"
  let sql =
        "CREATE TABLE workspace_membership \
        \( \
        \    workspace_uuid uuid        NOT NULL, \
        \    user_uuid      uuid        NOT NULL, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    created_at     timestamptz NOT NULL, \
        \    role_uuid      uuid        NOT NULL, \
        \    CONSTRAINT workspace_membership_pk PRIMARY KEY (workspace_uuid, user_uuid), \
        \    CONSTRAINT workspace_membership_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_membership_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_membership_role_uuid_fk FOREIGN KEY (role_uuid) REFERENCES role (uuid) \
        \); \
        \ \
        \CREATE INDEX workspace_membership_user_uuid_index ON workspace_membership (user_uuid, tenant_uuid); \
        \CREATE INDEX workspace_membership_role_uuid_index ON workspace_membership (role_uuid); \
        \ALTER TABLE workspace ADD CONSTRAINT workspace_default_role_uuid_fk FOREIGN KEY (default_role_uuid) REFERENCES role (uuid);"
  let action conn = execute_ conn sql
  runDB action
