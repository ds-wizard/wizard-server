module WizardServer.Database.Migration.Production.Migration_5_0_0.Migration (
  definition,
  meta,
  migrateShared,
  addWorkspaceColumn,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

import Shared.Util.String (f')

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5000000, mmName = "Workspaces", mmDescription = "Add workspaces, workspace memberships and the workspace column of the workspace-scoped tables"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migrateShared dbPool
  return Nothing

migrateShared :: Pool Connection -> LoggingT IO ()
migrateShared dbPool = do
  createWorkspaceTables dbPool
  addTenantMultiWorkspace dbPool
  insertWorkspaces dbPool
  insertWorkspaceMemberships dbPool
  addWorkspaceColumn dbPool "project"
  addWorkspaceColumn dbPool "document"
  addWorkspaceColumn dbPool "knowledge_model_editor"
  addWorkspaceColumn dbPool "user_group"
  addWorkspaceColumn dbPool "project_cache"
  addNullableWorkspaceColumn dbPool "knowledge_model_package"
  addNullableWorkspaceColumn dbPool "document_template"
  addNullableWorkspaceColumn dbPool "knowledge_model_secret"
  addNullableWorkspaceColumn dbPool "role"
  createUniqueConstraints dbPool
  createFunctions dbPool

createWorkspaceTables :: Pool Connection -> LoggingT IO ()
createWorkspaceTables dbPool = do
  let sql =
        "CREATE TABLE workspace ( \
        \    uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    CONSTRAINT workspace_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT workspace_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE INDEX workspace_tenant_uuid_index ON workspace (tenant_uuid); \
        \CREATE TABLE workspace_membership ( \
        \    workspace_uuid uuid NOT NULL, \
        \    user_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    CONSTRAINT workspace_membership_pk PRIMARY KEY (workspace_uuid, user_uuid), \
        \    CONSTRAINT workspace_membership_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_membership_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT workspace_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE INDEX workspace_membership_user_uuid_index ON workspace_membership (user_uuid, tenant_uuid);"
  runSql dbPool sql

addTenantMultiWorkspace :: Pool Connection -> LoggingT IO ()
addTenantMultiWorkspace dbPool = runSql dbPool "ALTER TABLE tenant ADD COLUMN multi_workspace boolean NOT NULL DEFAULT false;"

insertWorkspaces :: Pool Connection -> LoggingT IO ()
insertWorkspaces dbPool =
  runSql
    dbPool
    "INSERT INTO workspace (uuid, tenant_uuid, name, description, created_at, updated_at) \
    \SELECT gen_random_uuid(), uuid, name, NULL, now(), now() FROM tenant;"

insertWorkspaceMemberships :: Pool Connection -> LoggingT IO ()
insertWorkspaceMemberships dbPool =
  runSql
    dbPool
    "INSERT INTO workspace_membership (workspace_uuid, user_uuid, tenant_uuid, created_at) \
    \SELECT workspace.uuid, user_entity.uuid, user_entity.tenant_uuid, now() \
    \FROM user_entity \
    \JOIN workspace ON workspace.tenant_uuid = user_entity.tenant_uuid; \
    \INSERT INTO workspace_membership (workspace_uuid, user_uuid, tenant_uuid, created_at) \
    \SELECT uuid, '00000000-0000-0000-0000-000000000000', tenant_uuid, now() \
    \FROM workspace \
    \ON CONFLICT DO NOTHING;"

addWorkspaceColumn :: Pool Connection -> String -> LoggingT IO ()
addWorkspaceColumn dbPool table =
  runSql dbPool . fromString $
    f'
      "ALTER TABLE %s ADD COLUMN workspace_uuid uuid; \
      \UPDATE %s SET workspace_uuid = workspace.uuid FROM workspace WHERE workspace.tenant_uuid = %s.tenant_uuid; \
      \ALTER TABLE %s ALTER COLUMN workspace_uuid SET NOT NULL; \
      \ALTER TABLE %s ADD CONSTRAINT %s_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE; \
      \CREATE INDEX %s_workspace_uuid_index ON %s (workspace_uuid);"
      [table, table, table, table, table, table, table, table]

addNullableWorkspaceColumn :: Pool Connection -> String -> LoggingT IO ()
addNullableWorkspaceColumn dbPool table =
  runSql dbPool . fromString $
    f'
      "ALTER TABLE %s ADD COLUMN workspace_uuid uuid; \
      \ALTER TABLE %s ADD CONSTRAINT %s_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE; \
      \CREATE INDEX %s_workspace_uuid_index ON %s (workspace_uuid);"
      [table, table, table, table, table]

createUniqueConstraints :: Pool Connection -> LoggingT IO ()
createUniqueConstraints dbPool =
  runSql
    dbPool
    "ALTER TABLE knowledge_model_package DROP CONSTRAINT knowledge_model_package_coordinate_unique; \
    \ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_coordinate_unique UNIQUE NULLS NOT DISTINCT (organization_id, km_id, version, tenant_uuid, workspace_uuid); \
    \ALTER TABLE document_template ADD CONSTRAINT document_template_coordinate_unique UNIQUE NULLS NOT DISTINCT (organization_id, template_id, version, tenant_uuid, workspace_uuid);"

createFunctions :: Pool Connection -> LoggingT IO ()
createFunctions dbPool =
  runSql
    dbPool
    "DROP FUNCTION get_knowledge_model_editor_state; \
    \DROP FUNCTION get_newest_knowledge_model_package_coordinate; \
    \DROP FUNCTION get_newest_knowledge_model_package; \
    \CREATE FUNCTION get_newest_knowledge_model_package(req_organization_id varchar, req_km_id varchar, req_tenant_uuid uuid, req_phase varchar[], req_workspace_uuid uuid) \
    \    RETURNS uuid \
    \    LANGUAGE plpgsql \
    \AS \
    \$$ \
    \DECLARE \
    \    p_uuid uuid; \
    \BEGIN \
    \    SELECT uuid \
    \    INTO p_uuid \
    \    FROM knowledge_model_package \
    \    WHERE organization_id = req_organization_id \
    \      AND km_id = req_km_id \
    \      AND tenant_uuid = req_tenant_uuid \
    \      AND phase = ANY (req_phase) \
    \      AND (workspace_uuid IS NULL OR workspace_uuid = req_workspace_uuid) \
    \    ORDER BY (string_to_array(version, '.')::int[])[1] DESC, \
    \             (string_to_array(version, '.')::int[])[2] DESC, \
    \             (string_to_array(version, '.')::int[])[3] DESC, \
    \             workspace_uuid IS NOT NULL DESC \
    \    LIMIT 1; \
    \    RETURN p_uuid; \
    \END; \
    \$$; \
    \CREATE FUNCTION get_newest_knowledge_model_package_coordinate(req_coordinate varchar, req_tenant_uuid uuid, req_phase varchar[], req_workspace_uuid uuid) \
    \    RETURNS varchar \
    \    LANGUAGE plpgsql \
    \AS \
    \$$ \
    \DECLARE \
    \    target_uuid       uuid; \
    \    result_coordinate varchar; \
    \BEGIN \
    \    IF req_coordinate IS NULL THEN \
    \        RETURN NULL; \
    \    END IF; \
    \    target_uuid := get_newest_knowledge_model_package(get_organization_id(req_coordinate), get_km_id(req_coordinate), req_tenant_uuid, req_phase, req_workspace_uuid); \
    \    IF target_uuid IS NOT NULL THEN \
    \        SELECT concat(organization_id, ':', km_id, ':', version) \
    \        INTO result_coordinate \
    \        FROM knowledge_model_package \
    \        WHERE uuid = target_uuid; \
    \    END IF; \
    \    RETURN result_coordinate; \
    \END; \
    \$$; \
    \CREATE FUNCTION get_knowledge_model_editor_state(editor knowledge_model_editor, knowledge_model_migration knowledge_model_migration, fork_of_package_id varchar, editor_tenant_uuid uuid) \
    \    RETURNS varchar \
    \    LANGUAGE plpgsql \
    \AS \
    \$$ \
    \DECLARE \
    \    state varchar; \
    \BEGIN \
    \    SELECT CASE \
    \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
    \                    knowledge_model_migration.state ->> 'type' != 'CompletedKnowledgeModelMigrationState' THEN 'MigratingKnowledgeModelEditorState' \
    \               WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND \
    \                    knowledge_model_migration.state ->> 'type' = 'CompletedKnowledgeModelMigrationState' THEN 'MigratedKnowledgeModelEditorState' \
    \               WHEN (SELECT COUNT(*) FROM knowledge_model_editor_event editor_event WHERE editor_event.tenant_uuid = editor.tenant_uuid AND editor_event.editor_uuid = editor.uuid) > 0 THEN 'EditedKnowledgeModelEditorState' \
    \               WHEN fork_of_package_id != get_newest_knowledge_model_package_coordinate(fork_of_package_id, editor.tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase'], editor.workspace_uuid) THEN 'OutdatedKnowledgeModelEditorState' \
    \               WHEN True THEN 'DefaultKnowledgeModelEditorState' END \
    \    INTO state; \
    \    RETURN state; \
    \END; \
    \$$;"

runSql :: Pool Connection -> Query -> LoggingT IO ()
runSql dbPool sql = do
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()
