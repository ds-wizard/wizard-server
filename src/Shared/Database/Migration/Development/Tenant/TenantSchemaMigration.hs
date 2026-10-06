module Shared.Database.Migration.Development.Tenant.TenantSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Tenant) drop table"
  let sql =
        "DROP TABLE IF EXISTS tenant_limit_bundle; \
        \DROP TABLE IF EXISTS tenant CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropConfigTables :: WizardRequestContextC s m => m Int64
dropConfigTables = do
  logInfo _CMP_MIGRATION "(Table/Settings) drop table"
  let sql =
        "DROP TABLE IF EXISTS tenant_module; \
        \DROP TABLE IF EXISTS tenant_plugin_settings; \
        \DROP TABLE IF EXISTS config_mail; \
        \DROP TABLE IF EXISTS settings_features; \
        \DROP TABLE IF EXISTS settings_submission_service_supported_format; \
        \DROP TABLE IF EXISTS settings_submission_service_request_header; \
        \DROP TABLE IF EXISTS settings_submission_service; \
        \DROP TABLE IF EXISTS settings_submission; \
        \DROP TABLE IF EXISTS settings_projects; \
        \DROP TABLE IF EXISTS settings_registry; \
        \DROP TABLE IF EXISTS settings_look_and_feel; \
        \DROP TABLE IF EXISTS settings_dashboard_and_menu_custom_menu_link; \
        \DROP TABLE IF EXISTS settings_dashboard_and_menu_announcement; \
        \DROP TABLE IF EXISTS settings_dashboard_and_menu; \
        \DROP TABLE IF EXISTS settings_login_screen_announcement; \
        \DROP TABLE IF EXISTS settings_login_screen; \
        \DROP TABLE IF EXISTS settings_support; \
        \DROP TABLE IF EXISTS settings_roles; \
        \DROP TABLE IF EXISTS settings_users; \
        \DROP TABLE IF EXISTS settings_authentication; \
        \DROP TYPE IF EXISTS settings_announcement_level_type; \
        \DROP TABLE IF EXISTS config_owl; \
        \DROP TABLE IF EXISTS config_features; \
        \DROP TABLE IF EXISTS config_submission_service_supported_format; \
        \DROP TABLE IF EXISTS config_submission_service_request_header; \
        \DROP TABLE IF EXISTS config_submission_service; \
        \DROP TABLE IF EXISTS config_submission; \
        \DROP TABLE IF EXISTS config_project; \
        \DROP TABLE IF EXISTS config_registry; \
        \DROP TABLE IF EXISTS config_look_and_feel_custom_menu_link; \
        \DROP TABLE IF EXISTS config_look_and_feel; \
        \DROP TABLE IF EXISTS config_dashboard_and_login_screen_announcement; \
        \DROP TABLE IF EXISTS config_dashboard_and_login_screen; \
        \DROP TABLE IF EXISTS config_privacy_and_support; \
        \DROP TABLE IF EXISTS config_authentication; \
        \DROP TABLE IF EXISTS config_organization; \
        \DROP TYPE IF EXISTS config_dashboard_and_login_screen_announcement_type;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createTenantTable
  createTenantLimitBundleTable

createTenantTable :: WizardRequestContextC s m => m Int64
createTenantTable = do
  logInfo _CMP_MIGRATION "(Table/Tenant) create table"
  let sql =
        "CREATE TABLE tenant \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    tenant_id     varchar     NOT NULL, \
        \    name          varchar     NOT NULL, \
        \    server_domain varchar     NOT NULL, \
        \    client_url    varchar     NOT NULL, \
        \    enabled       bool        NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    updated_at    timestamptz NOT NULL, \
        \    server_url    varchar     NOT NULL, \
        \    state         varchar     NOT NULL DEFAULT 'ReadyForUseTenantState', \
        \    multi_workspace bool      NOT NULL DEFAULT false, \
        \    CONSTRAINT tenant_pk PRIMARY KEY (uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createConfigTables :: WizardRequestContextC s m => m Int64
createConfigTables = do
  createSettingsTables
  createTcMailTable
  createTenantPluginSettingsTable
  createTenantModuleTable

createSettingsTables :: WizardRequestContextC s m => m Int64
createSettingsTables = do
  logInfo _CMP_MIGRATION "(Table/Settings) create tables"
  let action conn = execute_ conn settingsTablesSql
  runDB action

settingsTablesSql :: Query
settingsTablesSql =
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

createTcMailTable :: WizardRequestContextC s m => m Int64
createTcMailTable = do
  logInfo _CMP_MIGRATION "(Table/ConfigMail) create tables"
  let sql =
        "CREATE TABLE config_mail \
        \( \
        \    tenant_uuid      uuid        NOT NULL, \
        \    config_uuid      uuid, \
        \    created_at       timestamptz NOT NULL, \
        \    updated_at       timestamptz NOT NULL, \
        \    custom_templates bool        NOT NULL, \
        \    CONSTRAINT config_mail_pk PRIMARY KEY (tenant_uuid), \
        \    CONSTRAINT config_mail_config_uuid_fk FOREIGN KEY (config_uuid) REFERENCES instance_config_mail (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT config_mail_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantLimitBundleTable :: WizardRequestContextC s m => m Int64
createTenantLimitBundleTable = do
  logInfo _CMP_MIGRATION "(Table/TenantLimitBundle) create table"
  let sql =
        "CREATE TABLE tenant_limit_bundle \
        \( \
        \    uuid                     uuid        NOT NULL, \
        \    users                    integer     NOT NULL, \
        \    active_users             integer     NOT NULL, \
        \    knowledge_models         integer     NOT NULL, \
        \    knowledge_model_editors  integer     NOT NULL, \
        \    document_templates       integer     NOT NULL, \
        \    projects                 integer     NOT NULL, \
        \    documents                integer     NOT NULL, \
        \    storage                  bigint      NOT NULL, \
        \    created_at               timestamptz NOT NULL, \
        \    updated_at               timestamptz NOT NULL, \
        \    document_template_drafts integer     NOT NULL, \
        \    locales                  integer     NOT NULL, \
        \    CONSTRAINT tenant_limit_bundle_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT tenant_limit_bundle_uuid_fk FOREIGN KEY (uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantModuleTable :: WizardRequestContextC s m => m Int64
createTenantModuleTable = do
  logInfo _CMP_MIGRATION "(Table/TenantModule) create table"
  let sql =
        "CREATE TABLE tenant_module \
        \( \
        \    tenant_uuid         uuid        NOT NULL, \
        \    position            int         NOT NULL, \
        \    module_key          varchar     NOT NULL, \
        \    title               varchar     NOT NULL, \
        \    description         varchar     NOT NULL, \
        \    icon                varchar     NOT NULL, \
        \    url                 varchar     NOT NULL, \
        \    external            bool        NOT NULL, \
        \    required_permission varchar, \
        \    enabled             bool        NOT NULL DEFAULT true, \
        \    created_at          timestamptz NOT NULL, \
        \    updated_at          timestamptz NOT NULL, \
        \    CONSTRAINT tenant_module_pk PRIMARY KEY (tenant_uuid, position), \
        \    CONSTRAINT tenant_module_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTenantPluginSettingsTable :: WizardRequestContextC s m => m Int64
createTenantPluginSettingsTable = do
  logInfo _CMP_MIGRATION "(Table/TenantPluginSettings) create tables"
  let sql =
        "CREATE TABLE tenant_plugin_settings \
        \( \
        \    tenant_uuid  uuid        NOT NULL, \
        \    plugin_uuid  uuid        NOT NULL, \
        \    values       jsonb       NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    updated_at   timestamptz NOT NULL, \
        \    CONSTRAINT tenant_plugin_settings_pk PRIMARY KEY (tenant_uuid, plugin_uuid), \
        \    CONSTRAINT tenant_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin (uuid, tenant_uuid) ON DELETE CASCADE, \
        \    CONSTRAINT tenant_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
