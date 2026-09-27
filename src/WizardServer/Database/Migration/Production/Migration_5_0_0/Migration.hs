module WizardServer.Database.Migration.Production.Migration_5_0_0.Migration (
  definition,
  meta,
  migrateShared,
  addWorkspaceColumn,
  renamePermissionsSql,
  renamePermissionSql,
  runSql,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import qualified Data.List as L
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

import Shared.Util.String (f')

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5000000, mmName = "Workspaces", mmDescription = "Add workspaces, workspace memberships, workspace roles and the workspace column of the workspace-scoped tables"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migrateShared dbPool
  return Nothing

migrateShared :: Pool Connection -> LoggingT IO ()
migrateShared dbPool = do
  assertNoDuplicateDocumentTemplates dbPool
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
  addRoleColumns dbPool
  renamePermissions dbPool
  seedWorkspaceRoles dbPool

assertNoDuplicateDocumentTemplates :: Pool Connection -> LoggingT IO ()
assertNoDuplicateDocumentTemplates dbPool =
  runSql
    dbPool
    "DO $$ \
    \DECLARE duplicates text; \
    \BEGIN \
    \    SELECT string_agg(concat_ws(':', organization_id, template_id, version, tenant_uuid), ', ') INTO duplicates \
    \    FROM (SELECT organization_id, template_id, version, tenant_uuid FROM document_template GROUP BY 1, 2, 3, 4 HAVING count(*) > 1) d; \
    \    IF duplicates IS NOT NULL THEN \
    \        RAISE EXCEPTION 'document_template holds duplicate coordinates, resolve them before upgrading: %', duplicates; \
    \    END IF; \
    \END $$;"

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

addRoleColumns :: Pool Connection -> LoggingT IO ()
addRoleColumns dbPool =
  runSql
    dbPool
    "ALTER TABLE workspace ADD COLUMN default_role_uuid uuid; \
    \ALTER TABLE workspace ADD CONSTRAINT workspace_default_role_uuid_fk FOREIGN KEY (default_role_uuid) REFERENCES role (uuid); \
    \ALTER TABLE workspace_membership ADD COLUMN role_uuid uuid; \
    \ALTER TABLE workspace_membership ADD CONSTRAINT workspace_membership_role_uuid_fk FOREIGN KEY (role_uuid) REFERENCES role (uuid); \
    \CREATE INDEX workspace_membership_role_uuid_index ON workspace_membership (role_uuid);"

renamePermissions :: Pool Connection -> LoggingT IO ()
renamePermissions dbPool =
  runSql dbPool . fromString $
    f'
      "UPDATE role SET permissions = %s; \
      \UPDATE user_entity SET role_permissions = %s; \
      \UPDATE role SET permissions = ARRAY(SELECT DISTINCT unnest(permissions || ARRAY['workspaces.manage', 'organizationLibrary.manage']))::varchar[] WHERE is_admin = true; \
      \UPDATE user_entity SET role_permissions = ARRAY(SELECT DISTINCT unnest(role_permissions || ARRAY['workspaces.manage', 'organizationLibrary.manage']))::varchar[] WHERE role_uuid IN (SELECT uuid FROM role WHERE is_admin = true);"
      [renamePermissionsSql "permissions", renamePermissionsSql "role_permissions"]

renamePermissionsSql :: String -> String
renamePermissionsSql column =
  f'
    "ARRAY(SELECT DISTINCT np FROM unnest(%s || ARRAY['projects.create']) AS p, unnest(CASE p %s ELSE ARRAY[p] END) AS np)::varchar[]"
    [column, unwords (fmap toCase permissionMapping)]
  where
    toCase (old, new) = f' "WHEN '%s' THEN ARRAY[%s]" [old, L.intercalate ", " (fmap (\n -> f' "'%s'" [n]) new)]

renamePermissionSql :: String -> String
renamePermissionSql column =
  f' "CASE %s %s ELSE %s END" [column, unwords (fmap toCase permissionMapping), column]
  where
    toCase (old, new) = f' "WHEN '%s' THEN '%s'" [old, head new]

permissionMapping :: [(String, [String])]
permissionMapping =
  [ ("DevUseRolePermission", ["dev.use"])
  , ("TenantsManageRolePermission", ["tenants.manage"])
  , ("HubspotUseRolePermission", ["hubspot.use"])
  , ("SettingsManageRolePermission", ["organizationSettings.manage", "settings.manage", "roles.manage"])
  , ("UsersManageRolePermission", ["users.manage", "members.manage", "userGroups.manage"])
  , ("DocumentTemplateEditorsUseRolePermission", ["documentTemplates.useEditor"])
  , ("DocumentTemplatesManageRolePermission", ["documentTemplates.manage"])
  , ("KnowledgeModelEditorsUseRolePermission", ["knowledgeModels.useEditor"])
  , ("KnowledgeModelsManageRolePermission", ["knowledgeModels.manage"])
  , ("ProjectTemplatesManageRolePermission", ["projects.manageTemplates"])
  , ("ProjectsCommentRolePermission", ["projects.comment"])
  , ("ProjectsEditRolePermission", ["projects.edit"])
  , ("ProjectsManageRolePermission", ["projects.manage"])
  , ("ProjectsViewRolePermission", ["projects.view"])
  , ("AnalyticsUseRolePermission", ["analytics.use"])
  , ("AuditLogUseRolePermission", ["auditLog.use"])
  , ("AutomationsManageRolePermission", ["automations.manage"])
  , ("IntegrationHubUseRolePermission", ["knowledgeModels.manage"])
  , ("McpUseRolePermission", ["mcp.connect"])
  ]

seedWorkspaceRoles :: Pool Connection -> LoggingT IO ()
seedWorkspaceRoles dbPool =
  runSql dbPool . fromString $
    f'
      "INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at, workspace_uuid) \
      \SELECT gen_random_uuid(), 'Admin', ARRAY[%s]::varchar[], false, tenant_uuid, now(), now(), uuid FROM workspace; \
      \INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at, workspace_uuid) \
      \SELECT gen_random_uuid(), 'User', ARRAY[]::varchar[], false, tenant_uuid, now(), now(), uuid FROM workspace; \
      \UPDATE workspace SET default_role_uuid = role.uuid FROM role WHERE role.workspace_uuid = workspace.uuid AND role.name = 'User'; \
      \UPDATE workspace_membership SET role_uuid = workspace.default_role_uuid FROM workspace WHERE workspace.uuid = workspace_membership.workspace_uuid; \
      \ALTER TABLE workspace_membership ALTER COLUMN role_uuid SET NOT NULL;"
      [L.intercalate ", " (fmap (\p -> f' "'%s'" [p]) workspaceAdminPermissions)]

workspaceAdminPermissions :: [String]
workspaceAdminPermissions =
  [ "projects.view"
  , "projects.comment"
  , "projects.edit"
  , "projects.manage"
  , "projects.create"
  , "projects.manageTemplates"
  , "knowledgeModels.useEditor"
  , "knowledgeModels.manage"
  , "documentTemplates.useEditor"
  , "documentTemplates.manage"
  , "members.manage"
  , "roles.manage"
  , "userGroups.manage"
  , "settings.manage"
  ]

runSql :: Pool Connection -> Query -> LoggingT IO ()
runSql dbPool sql = do
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()
