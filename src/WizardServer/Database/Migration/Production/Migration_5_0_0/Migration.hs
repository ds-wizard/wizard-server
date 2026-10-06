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

import Shared.Util.String (f', f'')

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5000000, mmName = "Workspaces", mmDescription = "Add workspaces, workspace memberships, workspace roles, the workspace column of the workspace-scoped tables, keep projects when their document template is deleted, replace organization id by a single id, replace registry organizations by accounts, rework tenant config into settings, add workspace branding and workspace plugin settings"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migrateShared dbPool
  return Nothing

migrateShared :: Pool Connection -> LoggingT IO ()
migrateShared dbPool = do
  assertNoDuplicateIds dbPool "knowledge_model_package" "km_id"
  assertNoDuplicateIds dbPool "document_template" "template_id"
  assertNoDuplicateIds dbPool "locale" "locale_id"
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
  replaceKnowledgeModelPackageOrganizationId dbPool
  replaceDocumentTemplateOrganizationId dbPool
  replaceLocaleOrganizationId dbPool
  replaceKnowledgeModelEditorKmId dbPool
  replaceRegistryOrganizationId dbPool "registry_knowledge_model_package" "km_id"
  replaceRegistryOrganizationId dbPool "registry_document_template" "template_id"
  replaceRegistryOrganizationId dbPool "registry_locale" "locale_id"
  dropConfigOrganizationId dbPool
  createUniqueConstraints dbPool
  createFunctions dbPool
  addRoleColumns dbPool
  addWorkspaceBranding dbPool
  renamePermissions dbPool
  seedWorkspaceRoles dbPool
  keepProjectsOnDocumentTemplateDelete dbPool
  dropConfigOwl dbPool
  replaceRegistryToken dbPool
  dropRegistryOrganization dbPool
  createSettingsTables dbPool
  copyConfigIntoSettings dbPool
  dropConfigTables dbPool
  createWorkspacePluginSettings dbPool

assertNoDuplicateIds :: Pool Connection -> String -> String -> LoggingT IO ()
assertNoDuplicateIds dbPool table entityIdColumn =
  runSql dbPool . fromString $
    f''
      "DO $$ \
      \DECLARE duplicates text; \
      \BEGIN \
      \    SELECT string_agg(concat_ws(':', id, version, tenant_uuid), ', ') INTO duplicates \
      \    FROM (SELECT concat(organization_id, '.', ${entityId}) AS id, version, tenant_uuid FROM ${table} GROUP BY 1, 2, 3 HAVING count(*) > 1) d; \
      \    IF duplicates IS NOT NULL THEN \
      \        RAISE EXCEPTION '${table} holds duplicate ids, resolve them before upgrading: %', duplicates; \
      \    END IF; \
      \END $$;"
      [("table", table), ("entityId", entityIdColumn)]

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

replaceKnowledgeModelPackageOrganizationId :: Pool Connection -> LoggingT IO ()
replaceKnowledgeModelPackageOrganizationId dbPool =
  runSql
    dbPool
    "ALTER TABLE knowledge_model_package ADD COLUMN fork_of_package_version varchar, ADD COLUMN merge_checkpoint_package_version varchar; \
    \UPDATE knowledge_model_package \
    \SET km_id = concat(organization_id, '.', km_id), \
    \    fork_of_package_id = split_part(fork_of_package_id, ':', 1) || '.' || split_part(fork_of_package_id, ':', 2), \
    \    fork_of_package_version = split_part(fork_of_package_id, ':', 3), \
    \    merge_checkpoint_package_id = split_part(merge_checkpoint_package_id, ':', 1) || '.' || split_part(merge_checkpoint_package_id, ':', 2), \
    \    merge_checkpoint_package_version = split_part(merge_checkpoint_package_id, ':', 3); \
    \DROP INDEX IF EXISTS knowledge_model_package_organization_id_km_id_index; \
    \ALTER TABLE knowledge_model_package DROP CONSTRAINT knowledge_model_package_coordinate_unique; \
    \ALTER TABLE knowledge_model_package DROP COLUMN organization_id; \
    \ALTER TABLE knowledge_model_package RENAME COLUMN km_id TO id; \
    \CREATE INDEX knowledge_model_package_id_index ON knowledge_model_package (id, tenant_uuid);"

replaceDocumentTemplateOrganizationId :: Pool Connection -> LoggingT IO ()
replaceDocumentTemplateOrganizationId dbPool =
  runSql
    dbPool
    "UPDATE document_template \
    \SET template_id = concat(organization_id, '.', template_id), \
    \    allowed_packages = (SELECT coalesce(jsonb_agg(jsonb_build_object( \
    \                                   'id', CASE WHEN rule ->> 'orgId' IS NOT NULL AND rule ->> 'kmId' IS NOT NULL THEN concat(rule ->> 'orgId', '.', rule ->> 'kmId') END, \
    \                                   'minVersion', rule -> 'minVersion', \
    \                                   'maxVersion', rule -> 'maxVersion') ORDER BY position), '[]'::jsonb) \
    \                        FROM jsonb_array_elements(allowed_packages) WITH ORDINALITY AS rules(rule, position)); \
    \DROP INDEX IF EXISTS document_template_organization_id_template_id_index; \
    \ALTER TABLE document_template DROP COLUMN organization_id; \
    \ALTER TABLE document_template RENAME COLUMN template_id TO id; \
    \CREATE INDEX document_template_id_index ON document_template (id, tenant_uuid);"

replaceLocaleOrganizationId :: Pool Connection -> LoggingT IO ()
replaceLocaleOrganizationId dbPool =
  runSql
    dbPool
    "UPDATE locale SET locale_id = concat(organization_id, '.', locale_id); \
    \ALTER TABLE locale DROP COLUMN organization_id; \
    \ALTER TABLE locale RENAME COLUMN locale_id TO id; \
    \ALTER TABLE locale ADD CONSTRAINT locale_id_unique UNIQUE (id, version, tenant_uuid); \
    \CREATE INDEX locale_id_index ON locale (id, tenant_uuid);"

replaceKnowledgeModelEditorKmId :: Pool Connection -> LoggingT IO ()
replaceKnowledgeModelEditorKmId dbPool =
  runSql
    dbPool
    "UPDATE knowledge_model_editor \
    \SET km_id = concat(config_organization.organization_id, '.', knowledge_model_editor.km_id) \
    \FROM config_organization \
    \WHERE config_organization.tenant_uuid = knowledge_model_editor.tenant_uuid; \
    \ALTER TABLE knowledge_model_editor RENAME COLUMN km_id TO id;"

replaceRegistryOrganizationId :: Pool Connection -> String -> String -> LoggingT IO ()
replaceRegistryOrganizationId dbPool table entityIdColumn =
  runSql dbPool . fromString $
    f''
      "UPDATE ${table} SET ${entityId} = concat(organization_id, '.', ${entityId}); \
      \ALTER TABLE ${table} DROP CONSTRAINT ${table}_pk; \
      \ALTER TABLE ${table} DROP COLUMN organization_id; \
      \ALTER TABLE ${table} RENAME COLUMN ${entityId} TO id; \
      \ALTER TABLE ${table} ADD CONSTRAINT ${table}_pk PRIMARY KEY (id);"
      [("table", table), ("entityId", entityIdColumn)]

dropConfigOrganizationId :: Pool Connection -> LoggingT IO ()
dropConfigOrganizationId dbPool = runSql dbPool "ALTER TABLE config_organization DROP COLUMN organization_id;"

createUniqueConstraints :: Pool Connection -> LoggingT IO ()
createUniqueConstraints dbPool =
  runSql
    dbPool
    "ALTER TABLE knowledge_model_package ADD CONSTRAINT knowledge_model_package_coordinate_unique UNIQUE NULLS NOT DISTINCT (id, version, tenant_uuid, workspace_uuid); \
    \ALTER TABLE document_template ADD CONSTRAINT document_template_coordinate_unique UNIQUE NULLS NOT DISTINCT (id, version, tenant_uuid, workspace_uuid);"

createFunctions :: Pool Connection -> LoggingT IO ()
createFunctions dbPool =
  runSql
    dbPool
    "DROP FUNCTION get_knowledge_model_editor_state; \
    \DROP FUNCTION get_knowledge_model_editor_fork_of_package_id; \
    \DROP FUNCTION get_newest_knowledge_model_package_coordinate; \
    \DROP FUNCTION get_newest_knowledge_model_package; \
    \DROP FUNCTION get_organization_id; \
    \DROP FUNCTION get_km_id; \
    \CREATE FUNCTION get_newest_knowledge_model_package(req_id varchar, req_tenant_uuid uuid, req_phase varchar[], req_workspace_uuid uuid) \
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
    \    WHERE id = req_id \
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
    \CREATE FUNCTION get_knowledge_model_editor_state(editor knowledge_model_editor, knowledge_model_migration knowledge_model_migration, fork_of_package_id varchar, fork_of_package_uuid uuid, editor_tenant_uuid uuid) \
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
    \               WHEN fork_of_package_uuid != get_newest_knowledge_model_package(fork_of_package_id, editor.tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase'], editor.workspace_uuid) THEN 'OutdatedKnowledgeModelEditorState' \
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

addWorkspaceBranding :: Pool Connection -> LoggingT IO ()
addWorkspaceBranding dbPool =
  runSql
    dbPool
    "ALTER TABLE workspace ADD COLUMN logo character varying; \
    \ALTER TABLE workspace ADD COLUMN primary_color character varying;"

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

keepProjectsOnDocumentTemplateDelete :: Pool Connection -> LoggingT IO ()
keepProjectsOnDocumentTemplateDelete dbPool =
  runSql
    dbPool
    "ALTER TABLE project \
    \    DROP CONSTRAINT IF EXISTS project_document_template_uuid_fk, \
    \    ADD CONSTRAINT project_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE SET NULL;"

dropConfigOwl :: Pool Connection -> LoggingT IO ()
dropConfigOwl dbPool = runSql dbPool "DROP TABLE IF EXISTS config_owl;"

replaceRegistryToken :: Pool Connection -> LoggingT IO ()
replaceRegistryToken dbPool = runSql dbPool "ALTER TABLE config_registry RENAME COLUMN token TO api_key;"

dropRegistryOrganization :: Pool Connection -> LoggingT IO ()
dropRegistryOrganization dbPool = runSql dbPool "DROP TABLE IF EXISTS registry_organization;"

createSettingsTables :: Pool Connection -> LoggingT IO ()
createSettingsTables dbPool =
  runSql
    dbPool
    "CREATE TYPE settings_announcement_level_type AS ENUM ('InfoAnnouncementLevelType', 'WarningAnnouncementLevelType', 'CriticalAnnouncementLevelType'); \
    \CREATE TABLE settings_authentication \
    \( \
    \    tenant_uuid                     uuid        NOT NULL, \
    \    registration_enabled            bool        NOT NULL, \
    \    non_admin_login_enabled         bool        NOT NULL, \
    \    session_expiration              bigint      NOT NULL, \
    \    user_email_link_expiration      bigint      NOT NULL, \
    \    two_factor_auth_enabled         bool        NOT NULL, \
    \    two_factor_auth_code_length     int         NOT NULL, \
    \    two_factor_auth_code_expiration int         NOT NULL, \
    \    created_at                      timestamptz NOT NULL, \
    \    updated_at                      timestamptz NOT NULL, \
    \    CONSTRAINT settings_authentication_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_authentication_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_users \
    \( \
    \    tenant_uuid  uuid        NOT NULL, \
    \    affiliations varchar[]   NOT NULL, \
    \    created_at   timestamptz NOT NULL, \
    \    updated_at   timestamptz NOT NULL, \
    \    CONSTRAINT settings_users_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_users_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_roles \
    \( \
    \    tenant_uuid       uuid        NOT NULL, \
    \    default_role_uuid uuid        NOT NULL, \
    \    created_at        timestamptz NOT NULL, \
    \    updated_at        timestamptz NOT NULL, \
    \    CONSTRAINT settings_roles_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_roles_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_login_screen \
    \( \
    \    tenant_uuid        uuid        NOT NULL, \
    \    login_info         varchar, \
    \    login_info_sidebar varchar, \
    \    created_at         timestamptz NOT NULL, \
    \    updated_at         timestamptz NOT NULL, \
    \    CONSTRAINT settings_login_screen_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_login_screen_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_login_screen_announcement \
    \( \
    \    tenant_uuid uuid                             NOT NULL, \
    \    position    int                              NOT NULL, \
    \    content     varchar                          NOT NULL, \
    \    level       settings_announcement_level_type NOT NULL, \
    \    CONSTRAINT settings_login_screen_announcement_pk PRIMARY KEY (tenant_uuid, position), \
    \    CONSTRAINT settings_login_screen_announcement_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_registry \
    \( \
    \    tenant_uuid uuid        NOT NULL, \
    \    enabled     boolean     NOT NULL, \
    \    api_key     varchar     NOT NULL, \
    \    created_at  timestamptz NOT NULL, \
    \    updated_at  timestamptz NOT NULL, \
    \    CONSTRAINT settings_registry_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_registry_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_features \
    \( \
    \    tenant_uuid          uuid        NOT NULL, \
    \    ai_assistant_enabled bool        NOT NULL, \
    \    tours_enabled        bool        NOT NULL, \
    \    created_at           timestamptz NOT NULL, \
    \    updated_at           timestamptz NOT NULL, \
    \    CONSTRAINT settings_features_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_features_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_look_and_feel \
    \( \
    \    tenant_uuid   uuid        NOT NULL, \
    \    app_title     varchar, \
    \    logo_url      varchar, \
    \    primary_color varchar, \
    \    created_at    timestamptz NOT NULL, \
    \    updated_at    timestamptz NOT NULL, \
    \    CONSTRAINT settings_look_and_feel_pk PRIMARY KEY (tenant_uuid), \
    \    CONSTRAINT settings_look_and_feel_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_dashboard_and_menu \
    \( \
    \    tenant_uuid                uuid        NOT NULL, \
    \    workspace_uuid             uuid, \
    \    workspace_override_allowed bool        NOT NULL DEFAULT true, \
    \    created_at                 timestamptz NOT NULL, \
    \    updated_at                 timestamptz NOT NULL, \
    \    CONSTRAINT settings_dashboard_and_menu_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid), \
    \    CONSTRAINT settings_dashboard_and_menu_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_dashboard_and_menu_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_dashboard_and_menu_custom_menu_link \
    \( \
    \    tenant_uuid    uuid    NOT NULL, \
    \    workspace_uuid uuid, \
    \    position       int     NOT NULL, \
    \    icon           varchar NOT NULL, \
    \    title          varchar NOT NULL, \
    \    url            varchar NOT NULL, \
    \    new_window     bool    NOT NULL, \
    \    CONSTRAINT settings_dashboard_and_menu_custom_menu_link_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, position), \
    \    CONSTRAINT settings_dashboard_and_menu_custom_menu_link_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_dashboard_and_menu_custom_menu_link_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_dashboard_and_menu_announcement \
    \( \
    \    tenant_uuid    uuid                             NOT NULL, \
    \    workspace_uuid uuid, \
    \    position       int                              NOT NULL, \
    \    content        varchar                          NOT NULL, \
    \    level          settings_announcement_level_type NOT NULL, \
    \    CONSTRAINT settings_dashboard_and_menu_announcement_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, position), \
    \    CONSTRAINT settings_dashboard_and_menu_announcement_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_dashboard_and_menu_announcement_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_projects \
    \( \
    \    tenant_uuid                uuid        NOT NULL, \
    \    workspace_uuid             uuid, \
    \    workspace_override_allowed bool        NOT NULL DEFAULT true, \
    \    visibility_enabled         boolean     NOT NULL, \
    \    visibility_default_value   varchar     NOT NULL, \
    \    sharing_enabled            boolean     NOT NULL, \
    \    sharing_default_value      varchar     NOT NULL, \
    \    sharing_anonymous_enabled  boolean     NOT NULL, \
    \    creation                   varchar     NOT NULL, \
    \    project_tagging_enabled    boolean     NOT NULL, \
    \    project_tagging_tags       varchar[]   NOT NULL, \
    \    summary_report             boolean     NOT NULL, \
    \    created_at                 timestamptz NOT NULL, \
    \    updated_at                 timestamptz NOT NULL, \
    \    CONSTRAINT settings_projects_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid), \
    \    CONSTRAINT settings_projects_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_projects_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_support \
    \( \
    \    tenant_uuid                uuid        NOT NULL, \
    \    workspace_uuid             uuid, \
    \    workspace_override_allowed bool        NOT NULL DEFAULT true, \
    \    support_email              varchar, \
    \    support_site_name          varchar, \
    \    support_site_url           varchar, \
    \    support_site_icon          varchar, \
    \    created_at                 timestamptz NOT NULL, \
    \    updated_at                 timestamptz NOT NULL, \
    \    CONSTRAINT settings_support_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid), \
    \    CONSTRAINT settings_support_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_support_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_submission \
    \( \
    \    tenant_uuid                uuid        NOT NULL, \
    \    workspace_uuid             uuid, \
    \    workspace_override_allowed bool        NOT NULL DEFAULT true, \
    \    enabled                    boolean     NOT NULL, \
    \    created_at                 timestamptz NOT NULL, \
    \    updated_at                 timestamptz NOT NULL, \
    \    CONSTRAINT settings_submission_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid), \
    \    CONSTRAINT settings_submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_submission_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_submission_service \
    \( \
    \    tenant_uuid                 uuid      NOT NULL, \
    \    workspace_uuid              uuid, \
    \    id                          varchar   NOT NULL, \
    \    name                        varchar   NOT NULL, \
    \    description                 varchar   NOT NULL, \
    \    props                       varchar[] NOT NULL, \
    \    request_method              varchar   NOT NULL, \
    \    request_url                 varchar   NOT NULL, \
    \    request_multipart_enabled   boolean   NOT NULL, \
    \    request_multipart_file_name varchar   NOT NULL, \
    \    CONSTRAINT settings_submission_service_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, id), \
    \    CONSTRAINT settings_submission_service_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_submission_service_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_submission_service_request_header \
    \( \
    \    tenant_uuid    uuid    NOT NULL, \
    \    workspace_uuid uuid, \
    \    service_id     varchar NOT NULL, \
    \    name           varchar NOT NULL, \
    \    value          varchar NOT NULL, \
    \    CONSTRAINT settings_submission_service_request_header_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, service_id, name), \
    \    CONSTRAINT settings_submission_service_request_header_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_submission_service_request_header_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \); \
    \CREATE TABLE settings_submission_service_supported_format \
    \( \
    \    tenant_uuid    uuid    NOT NULL, \
    \    workspace_uuid uuid, \
    \    service_id     varchar NOT NULL, \
    \    id             varchar NOT NULL, \
    \    version        varchar NOT NULL, \
    \    format_name    varchar NOT NULL, \
    \    CONSTRAINT settings_submission_service_supported_format_key UNIQUE NULLS NOT DISTINCT (tenant_uuid, workspace_uuid, service_id, id, version, format_name), \
    \    CONSTRAINT settings_submission_service_supported_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT settings_submission_service_supported_format_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE \
    \);"

copyConfigIntoSettings :: Pool Connection -> LoggingT IO ()
copyConfigIntoSettings dbPool =
  runSql
    dbPool
    "INSERT INTO settings_authentication (tenant_uuid, registration_enabled, non_admin_login_enabled, session_expiration, user_email_link_expiration, two_factor_auth_enabled, two_factor_auth_code_length, two_factor_auth_code_expiration, created_at, updated_at) \
    \SELECT tenant_uuid, internal_registration_enabled, internal_non_admin_login_enabled, internal_session_expiration, internal_user_email_link_expiration, internal_two_factor_auth_enabled, internal_two_factor_auth_code_length, internal_two_factor_auth_code_expiration, created_at, updated_at FROM config_authentication; \
    \INSERT INTO settings_roles (tenant_uuid, default_role_uuid, created_at, updated_at) \
    \SELECT tenant_uuid, default_role_uuid, created_at, updated_at FROM config_authentication; \
    \INSERT INTO settings_users (tenant_uuid, affiliations, created_at, updated_at) \
    \SELECT tenant_uuid, affiliations, created_at, updated_at FROM config_organization; \
    \INSERT INTO settings_login_screen (tenant_uuid, login_info, login_info_sidebar, created_at, updated_at) \
    \SELECT tenant_uuid, login_info, login_info_sidebar, created_at, updated_at FROM config_dashboard_and_login_screen; \
    \INSERT INTO settings_login_screen_announcement (tenant_uuid, position, content, level) \
    \SELECT tenant_uuid, position, content, level::text::settings_announcement_level_type FROM config_dashboard_and_login_screen_announcement WHERE login_screen; \
    \INSERT INTO settings_dashboard_and_menu (tenant_uuid, workspace_uuid, workspace_override_allowed, created_at, updated_at) \
    \SELECT tenant_uuid, NULL, true, created_at, updated_at FROM config_dashboard_and_login_screen; \
    \INSERT INTO settings_dashboard_and_menu_announcement (tenant_uuid, workspace_uuid, position, content, level) \
    \SELECT tenant_uuid, NULL, position, content, level::text::settings_announcement_level_type FROM config_dashboard_and_login_screen_announcement WHERE dashboard; \
    \INSERT INTO settings_dashboard_and_menu_custom_menu_link (tenant_uuid, workspace_uuid, position, icon, title, url, new_window) \
    \SELECT tenant_uuid, NULL, position, icon, title, url, new_window FROM config_look_and_feel_custom_menu_link; \
    \INSERT INTO settings_look_and_feel (tenant_uuid, app_title, logo_url, primary_color, created_at, updated_at) \
    \SELECT tenant_uuid, app_title, logo_url, primary_color, created_at, updated_at FROM config_look_and_feel; \
    \INSERT INTO settings_registry (tenant_uuid, enabled, api_key, created_at, updated_at) \
    \SELECT tenant_uuid, enabled, api_key, created_at, updated_at FROM config_registry; \
    \INSERT INTO settings_features (tenant_uuid, ai_assistant_enabled, tours_enabled, created_at, updated_at) \
    \SELECT tenant_uuid, ai_assistant_enabled, tours_enabled, created_at, updated_at FROM config_features; \
    \INSERT INTO settings_support (tenant_uuid, workspace_uuid, workspace_override_allowed, support_email, support_site_name, support_site_url, support_site_icon, created_at, updated_at) \
    \SELECT tenant_uuid, NULL, true, support_email, support_site_name, support_site_url, support_site_icon, created_at, updated_at FROM config_privacy_and_support; \
    \INSERT INTO settings_projects (tenant_uuid, workspace_uuid, workspace_override_allowed, visibility_enabled, visibility_default_value, sharing_enabled, sharing_default_value, sharing_anonymous_enabled, creation, project_tagging_enabled, project_tagging_tags, summary_report, created_at, updated_at) \
    \SELECT tenant_uuid, NULL, true, visibility_enabled, visibility_default_value, sharing_enabled, sharing_default_value, sharing_anonymous_enabled, creation, project_tagging_enabled, project_tagging_tags, summary_report, created_at, updated_at FROM config_project; \
    \INSERT INTO settings_submission (tenant_uuid, workspace_uuid, workspace_override_allowed, enabled, created_at, updated_at) \
    \SELECT tenant_uuid, NULL, true, enabled, created_at, updated_at FROM config_submission; \
    \INSERT INTO settings_submission_service (tenant_uuid, workspace_uuid, id, name, description, props, request_method, request_url, request_multipart_enabled, request_multipart_file_name) \
    \SELECT tenant_uuid, NULL, id, name, description, props, request_method, request_url, request_multipart_enabled, request_multipart_file_name FROM config_submission_service; \
    \INSERT INTO settings_submission_service_request_header (tenant_uuid, workspace_uuid, service_id, name, value) \
    \SELECT tenant_uuid, NULL, service_id, name, value FROM config_submission_service_request_header; \
    \INSERT INTO settings_submission_service_supported_format (tenant_uuid, workspace_uuid, service_id, id, version, format_name) \
    \SELECT DISTINCT supported_format.tenant_uuid, NULL::uuid, supported_format.service_id, template.id, template.version, format.name \
    \FROM config_submission_service_supported_format supported_format \
    \JOIN document_template template ON template.uuid = supported_format.document_template_uuid \
    \JOIN document_template_format format ON format.document_template_uuid = supported_format.document_template_uuid AND format.uuid = supported_format.format_uuid;"

dropConfigTables :: Pool Connection -> LoggingT IO ()
dropConfigTables dbPool =
  runSql
    dbPool
    "ALTER TABLE submission DROP CONSTRAINT IF EXISTS submission_service_id_fk; \
    \ALTER TABLE user_entity_submission_prop DROP CONSTRAINT IF EXISTS user_entity_submission_prop_service_id_fk; \
    \DROP TABLE config_submission_service_supported_format; \
    \DROP TABLE config_submission_service_request_header; \
    \DROP TABLE config_submission_service; \
    \DROP TABLE config_submission; \
    \DROP TABLE config_project; \
    \DROP TABLE config_registry; \
    \DROP TABLE config_features; \
    \DROP TABLE config_look_and_feel_custom_menu_link; \
    \DROP TABLE config_look_and_feel; \
    \DROP TABLE config_dashboard_and_login_screen_announcement; \
    \DROP TABLE config_dashboard_and_login_screen; \
    \DROP TABLE config_privacy_and_support; \
    \DROP TABLE config_authentication; \
    \DROP TABLE config_organization; \
    \DROP TYPE config_dashboard_and_login_screen_announcement_type;"

createWorkspacePluginSettings :: Pool Connection -> LoggingT IO ()
createWorkspacePluginSettings dbPool =
  runSql
    dbPool
    "ALTER TABLE plugin ADD COLUMN workspace_override_allowed boolean NOT NULL DEFAULT true; \
    \CREATE TABLE workspace_plugin_settings \
    \( \
    \    workspace_uuid uuid        NOT NULL, \
    \    plugin_uuid    uuid        NOT NULL, \
    \    tenant_uuid    uuid        NOT NULL, \
    \    enabled        boolean     NOT NULL DEFAULT true, \
    \    values         jsonb, \
    \    created_at     timestamptz NOT NULL, \
    \    updated_at     timestamptz NOT NULL, \
    \    CONSTRAINT workspace_plugin_settings_pk PRIMARY KEY (workspace_uuid, plugin_uuid), \
    \    CONSTRAINT workspace_plugin_settings_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE, \
    \    CONSTRAINT workspace_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin (uuid, tenant_uuid) ON DELETE CASCADE, \
    \    CONSTRAINT workspace_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
    \);"

runSql :: Pool Connection -> Query -> LoggingT IO ()
runSql dbPool sql = do
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()
