module WizardServer.Database.Migration.Production.Migration_4_35_0.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 4035000, mmName = "Init", mmDescription = "Create the initial database schema"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  assertEmptySchema dbPool
  createTypes dbPool
  createFunctions1 dbPool
  createTables1 dbPool
  createFunctions2 dbPool
  createTables2 dbPool
  createFunctions3 dbPool
  createTables3 dbPool
  createConstraints dbPool
  createIndexes dbPool
  createTriggers dbPool
  createForeignKeys dbPool
  insertTenant dbPool
  insertRoles dbPool
  insertUsers dbPool
  insertTenantLimitBundle dbPool
  insertLocale dbPool
  insertConfig dbPool
  return Nothing

assertEmptySchema :: Pool Connection -> LoggingT IO ()
assertEmptySchema dbPool = do
  let sql =
        "DO $$ \
        \BEGIN \
        \    IF EXISTS (SELECT 1 FROM pg_tables \
        \                WHERE schemaname = current_schema() AND tablename <> 'migration') THEN \
        \        RAISE EXCEPTION 'the init migration builds the schema from nothing and this database is not empty; an existing installation is upgraded by the upgrade script for this release, not by starting the new version'; \
        \    END IF; \
        \END $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTypes :: Pool Connection -> LoggingT IO ()
createTypes dbPool = do
  let sql =
        "CREATE TYPE config_dashboard_and_login_screen_announcement_type AS ENUM ( \
        \    'InfoAnnouncementLevelType', \
        \    'WarningAnnouncementLevelType', \
        \    'CriticalAnnouncementLevelType' \
        \); \
        \CREATE TYPE knowledge_model_editor_reply_type AS ENUM ( \
        \    'StringReply', \
        \    'AnswerReply', \
        \    'ItemSelectReply', \
        \    'FileReply', \
        \    'IntegrationReply', \
        \    'MultiChoiceReply', \
        \    'ItemListReply' \
        \); \
        \CREATE TYPE project_event_type AS ENUM ( \
        \    'ClearReplyEvent', \
        \    'SetReplyEvent', \
        \    'SetLabelsEvent', \
        \    'SetPhaseEvent' \
        \); \
        \CREATE TYPE sem_ver_2_tuple AS ( \
        \ major integer, \
        \ minor integer \
        \); \
        \CREATE TYPE value_type AS ENUM ( \
        \    'IntegrationReply', \
        \    'AnswerReply', \
        \    'MultiChoiceReply', \
        \    'ItemListReply', \
        \    'StringReply', \
        \    'ItemSelectReply', \
        \    'FileReply' \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createFunctions1 :: Pool Connection -> LoggingT IO ()
createFunctions1 dbPool = do
  let sql =
        "CREATE FUNCTION compare_version(version_1 character varying, version_2 character varying) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     version_order varchar; BEGIN     SELECT CASE                WHEN major_version(version_1) = major_version(version_2)                    THEN CASE                             WHEN minor_version(version_1) = minor_version(version_2)                                 THEN CASE                                          WHEN patch_version(version_1) = patch_version(version_2) THEN 'EQ'                                          WHEN patch_version(version_1) < patch_version(version_2) THEN 'LT'                                          WHEN patch_version(version_1) > patch_version(version_2) THEN 'GT'                                 END                             WHEN minor_version(version_1) < minor_version(version_2) THEN 'LT'                             WHEN minor_version(version_1) > minor_version(version_2) THEN 'GT'                    END                WHEN major_version(version_1) < major_version(version_2) THEN 'LT'                WHEN major_version(version_1) > major_version(version_2) THEN 'GT'                END     INTO version_order;     RETURN version_order; END; $$; \
        \CREATE FUNCTION create_persistent_command(component character varying, function character varying, body jsonb, tenant_uuid uuid) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ BEGIN     INSERT INTO persistent_command (uuid,                                     state,                                     component,                                     function,                                     body,                                     last_error_message,                                     attempts,                                     max_attempts,                                     tenant_uuid,                                     created_by,                                     created_at,                                     updated_at,                                     last_trace_uuid)     VALUES (gen_random_uuid(),             'NewPersistentCommandState',             component,             function,             body,             NULL,             0,             10,             tenant_uuid,             NULL,             now(),             now(),             NULL);     return 1; END; $$; \
        \CREATE FUNCTION create_persistent_command_from_document_delete() RETURNS trigger \
        \    LANGUAGE plpgsql \
        \    AS $$ BEGIN     PERFORM create_persistent_command(             'document',             'deleteFromS3',             jsonb_build_object('uuid', OLD.uuid),             OLD.tenant_uuid);     RETURN OLD; END; $$; \
        \CREATE FUNCTION create_persistent_command_from_document_template_asset_delete() RETURNS trigger \
        \    LANGUAGE plpgsql \
        \    AS $$ BEGIN     PERFORM create_persistent_command(             'document_template_asset',             'deleteFromS3',             jsonb_build_object('documentTemplateUuid', OLD.document_template_uuid, 'assetUuid', OLD.uuid),             OLD.tenant_uuid);     RETURN OLD; END; $$; \
        \CREATE FUNCTION create_persistent_command_from_entity_uuid() RETURNS trigger \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     component varchar;     function  varchar; BEGIN     component := TG_ARGV[0];     function := TG_ARGV[1];      PERFORM create_persistent_command(             component,             function,             jsonb_build_object('uuid', OLD.uuid),             OLD.tenant_uuid);     RETURN OLD; END; $$; \
        \CREATE FUNCTION create_persistent_command_from_project_file_delete() RETURNS trigger \
        \    LANGUAGE plpgsql \
        \    AS $$ BEGIN     PERFORM create_persistent_command(             'project_file',             'deleteFromS3',             jsonb_build_object('projectUuid', OLD.project_uuid, 'fileUuid', OLD.uuid),             OLD.tenant_uuid);     RETURN OLD; END; $$; \
        \CREATE FUNCTION get_km_id(req_p_id character varying) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     km_id varchar; BEGIN     SELECT split_part(req_p_id, ':', 2)     INTO km_id;     RETURN km_id;END; $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTables1 :: Pool Connection -> LoggingT IO ()
createTables1 dbPool = do
  let sql =
        "CREATE TABLE config_organization ( \
        \    tenant_uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    affiliations character varying[] NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_editor ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    km_id character varying NOT NULL, \
        \    previous_package_uuid uuid, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    version character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    license character varying NOT NULL, \
        \    metamodel_version integer NOT NULL, \
        \    squashed boolean NOT NULL, \
        \    language character varying DEFAULT 'en'::character varying NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_package ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    km_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    metamodel_version integer NOT NULL, \
        \    description character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    license character varying NOT NULL, \
        \    previous_package_uuid uuid, \
        \    fork_of_package_id character varying, \
        \    merge_checkpoint_package_id character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    phase character varying NOT NULL, \
        \    non_editable boolean NOT NULL, \
        \    public boolean NOT NULL, \
        \    language character varying DEFAULT 'en'::character varying NOT NULL \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createFunctions2 :: Pool Connection -> LoggingT IO ()
createFunctions2 dbPool = do
  let sql =
        "CREATE FUNCTION get_knowledge_model_editor_fork_of_package_id(config_organization config_organization, previous_pkg knowledge_model_package, knowledge_model_editor knowledge_model_editor) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     fork_of_package_id varchar; BEGIN     SELECT CASE                WHEN knowledge_model_editor.previous_package_uuid IS NULL THEN NULL                WHEN previous_pkg.organization_id = config_organization.organization_id AND                     previous_pkg.km_id = knowledge_model_editor.km_id THEN previous_pkg.fork_of_package_id                WHEN True THEN concat(previous_pkg.organization_id, ':', previous_pkg.km_id, ':', previous_pkg.version) END as fork_of_package_id     INTO fork_of_package_id;     RETURN fork_of_package_id; END; $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTables2 :: Pool Connection -> LoggingT IO ()
createTables2 dbPool = do
  let sql =
        "CREATE TABLE knowledge_model_migration ( \
        \    editor_uuid uuid NOT NULL, \
        \    metamodel_version integer NOT NULL, \
        \    state jsonb NOT NULL, \
        \    editor_previous_package_uuid uuid NOT NULL, \
        \    target_package_uuid uuid NOT NULL, \
        \    editor_previous_package_events jsonb NOT NULL, \
        \    target_package_events jsonb NOT NULL, \
        \    result_events jsonb NOT NULL, \
        \    current_knowledge_model jsonb, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createFunctions3 :: Pool Connection -> LoggingT IO ()
createFunctions3 dbPool = do
  let sql =
        "CREATE FUNCTION get_knowledge_model_editor_state(editor knowledge_model_editor, knowledge_model_migration knowledge_model_migration, fork_of_package_id character varying, editor_tenant_uuid uuid) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     state varchar; BEGIN     SELECT CASE                WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND                     knowledge_model_migration.state ->> 'type' != 'CompletedKnowledgeModelMigrationState' THEN 'MigratingKnowledgeModelEditorState'                WHEN knowledge_model_migration.state ->> 'type' IS NOT NULL AND                     knowledge_model_migration.state ->> 'type' = 'CompletedKnowledgeModelMigrationState' THEN 'MigratedKnowledgeModelEditorState'                WHEN (SELECT COUNT(*) FROM knowledge_model_editor_event editor_event WHERE editor_event.tenant_uuid = editor.tenant_uuid AND editor_event.editor_uuid = editor.uuid) > 0 THEN 'EditedKnowledgeModelEditorState'                WHEN fork_of_package_id != get_newest_knowledge_model_package_coordinate(fork_of_package_id, editor.tenant_uuid, ARRAY['ReleasedKnowledgeModelPackagePhase', 'DeprecatedKnowledgeModelPackagePhase']) THEN 'OutdatedKnowledgeModelEditorState'                WHEN True THEN 'DefaultKnowledgeModelEditorState' END     INTO state;     RETURN state; END; $$; \
        \CREATE FUNCTION get_newest_knowledge_model_package(req_organization_id character varying, req_km_id character varying, req_tenant_uuid uuid, req_phase character varying[]) RETURNS uuid \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     p_uuid uuid; BEGIN     SELECT uuid     INTO p_uuid     FROM knowledge_model_package     WHERE organization_id = req_organization_id       AND km_id = req_km_id       AND tenant_uuid = req_tenant_uuid       AND phase = ANY (req_phase)     ORDER BY (string_to_array(version, '.')::int[])[1] DESC,              (string_to_array(version, '.')::int[])[2] DESC,              (string_to_array(version, '.')::int[])[3] DESC     LIMIT 1;      RETURN p_uuid; END; $$; \
        \CREATE FUNCTION get_newest_knowledge_model_package_coordinate(req_coordinate character varying, req_tenant_uuid uuid, req_phase character varying[]) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     target_uuid       uuid;     result_coordinate varchar; BEGIN     IF req_coordinate IS NULL THEN         RETURN NULL;     END IF;      target_uuid := get_newest_knowledge_model_package(             get_organization_id(req_coordinate),             get_km_id(req_coordinate),             req_tenant_uuid,             req_phase                    );      IF target_uuid IS NOT NULL THEN         SELECT concat(organization_id, ':', km_id, ':', version)         INTO result_coordinate         FROM knowledge_model_package         WHERE uuid = target_uuid;     END IF;      RETURN result_coordinate; END; $$; \
        \CREATE FUNCTION get_organization_id(req_p_id character varying) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     organization_id varchar; BEGIN     SELECT split_part(req_p_id, ':', 1)     INTO organization_id;     RETURN organization_id; END; $$; \
        \CREATE FUNCTION gravatar_hash(email character varying) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     hash VARCHAR; BEGIN     SELECT md5(lower(trim(email)))     INTO hash;     RETURN hash; END; $$; \
        \CREATE FUNCTION is_outdated(version_1 character varying, version_2 character varying) RETURNS boolean \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     outdated varchar; BEGIN     SELECT CASE                WHEN compare_version(version_1, version_2) = 'GT' THEN true                ELSE false                END     INTO outdated;     RETURN outdated; END; $$; \
        \CREATE FUNCTION major_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     major_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[1]     INTO major_version;     RETURN major_version; END; $$; \
        \CREATE FUNCTION minor_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     minor_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[2]     INTO minor_version;     RETURN minor_version; END; $$; \
        \CREATE FUNCTION patch_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     patch_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[3]     INTO patch_version;     RETURN patch_version; END; $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTables3 :: Pool Connection -> LoggingT IO ()
createTables3 dbPool = do
  let sql =
        "CREATE TABLE audit ( \
        \    uuid uuid NOT NULL, \
        \    component character varying NOT NULL, \
        \    action character varying NOT NULL, \
        \    entity character varying NOT NULL, \
        \    body jsonb NOT NULL, \
        \    created_by uuid, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE component ( \
        \    name character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    built_at timestamp with time zone NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_authentication ( \
        \    tenant_uuid uuid NOT NULL, \
        \    default_role_uuid uuid NOT NULL, \
        \    internal_registration_enabled boolean NOT NULL, \
        \    internal_two_factor_auth_enabled boolean NOT NULL, \
        \    internal_two_factor_auth_code_length integer NOT NULL, \
        \    internal_two_factor_auth_code_expiration integer NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    internal_non_admin_login_enabled boolean NOT NULL, \
        \    internal_session_expiration bigint NOT NULL, \
        \    internal_user_email_link_expiration bigint NOT NULL \
        \); \
        \CREATE TABLE config_dashboard_and_login_screen ( \
        \    tenant_uuid uuid NOT NULL, \
        \    dashboard_type character varying NOT NULL, \
        \    login_info character varying, \
        \    login_info_sidebar character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_dashboard_and_login_screen_announcement ( \
        \    tenant_uuid uuid NOT NULL, \
        \    \"position\" integer NOT NULL, \
        \    content character varying NOT NULL, \
        \    level config_dashboard_and_login_screen_announcement_type NOT NULL, \
        \    login_screen boolean NOT NULL, \
        \    dashboard boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_features ( \
        \    tenant_uuid uuid NOT NULL, \
        \    ai_assistant_enabled boolean NOT NULL, \
        \    tours_enabled boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_look_and_feel ( \
        \    tenant_uuid uuid NOT NULL, \
        \    app_title character varying, \
        \    app_title_short character varying, \
        \    logo_url character varying, \
        \    primary_color character varying, \
        \    illustrations_color character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_look_and_feel_custom_menu_link ( \
        \    tenant_uuid uuid NOT NULL, \
        \    \"position\" integer NOT NULL, \
        \    icon character varying NOT NULL, \
        \    title character varying NOT NULL, \
        \    url character varying NOT NULL, \
        \    new_window boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_mail ( \
        \    tenant_uuid uuid NOT NULL, \
        \    config_uuid uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    custom_templates boolean NOT NULL \
        \); \
        \CREATE TABLE config_owl ( \
        \    tenant_uuid uuid NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    name character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    km_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    previous_package_uuid uuid, \
        \    root_element character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_privacy_and_support ( \
        \    tenant_uuid uuid NOT NULL, \
        \    privacy_url character varying, \
        \    terms_of_service_url character varying, \
        \    support_email character varying, \
        \    support_site_name character varying, \
        \    support_site_url character varying, \
        \    support_site_icon character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_project ( \
        \    tenant_uuid uuid NOT NULL, \
        \    visibility_enabled boolean NOT NULL, \
        \    visibility_default_value character varying NOT NULL, \
        \    sharing_enabled boolean NOT NULL, \
        \    sharing_default_value character varying NOT NULL, \
        \    sharing_anonymous_enabled boolean NOT NULL, \
        \    creation character varying NOT NULL, \
        \    project_tagging_enabled boolean NOT NULL, \
        \    project_tagging_tags character varying[] NOT NULL, \
        \    summary_report boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_registry ( \
        \    tenant_uuid uuid NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    token character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_submission ( \
        \    tenant_uuid uuid NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_submission_service ( \
        \    tenant_uuid uuid NOT NULL, \
        \    id character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    props character varying[] NOT NULL, \
        \    request_method character varying NOT NULL, \
        \    request_url character varying NOT NULL, \
        \    request_multipart_enabled boolean NOT NULL, \
        \    request_multipart_file_name character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE config_submission_service_request_header ( \
        \    tenant_uuid uuid NOT NULL, \
        \    service_id character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    value character varying NOT NULL \
        \); \
        \CREATE TABLE config_submission_service_supported_format ( \
        \    tenant_uuid uuid NOT NULL, \
        \    service_id character varying NOT NULL, \
        \    document_template_uuid uuid NOT NULL, \
        \    format_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE document ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    state character varying NOT NULL, \
        \    durability character varying NOT NULL, \
        \    project_uuid uuid, \
        \    project_event_uuid uuid, \
        \    project_replies_hash bigint NOT NULL, \
        \    document_template_uuid uuid NOT NULL, \
        \    format_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    retrieved_at timestamp with time zone, \
        \    finished_at timestamp with time zone, \
        \    created_at timestamp with time zone NOT NULL, \
        \    file_name character varying, \
        \    content_type character varying, \
        \    worker_log character varying, \
        \    tenant_uuid uuid NOT NULL, \
        \    file_size bigint, \
        \    language varchar \
        \); \
        \CREATE TABLE document_template ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    template_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    metamodel_version sem_ver_2_tuple NOT NULL, \
        \    description character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    license character varying NOT NULL, \
        \    allowed_packages jsonb NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    phase character varying NOT NULL, \
        \    non_editable boolean NOT NULL, \
        \    language varchar DEFAULT 'en' NOT NULL, \
        \    pot_file_ready boolean DEFAULT false NOT NULL \
        \); \
        \CREATE TABLE document_template_asset ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content_type character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    file_size bigint NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template_draft_data ( \
        \    document_template_uuid uuid NOT NULL, \
        \    project_uuid uuid, \
        \    format_uuid uuid, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    knowledge_model_editor_uuid uuid \
        \); \
        \CREATE TABLE document_template_file ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template_format ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    icon character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template_format_step ( \
        \    document_template_uuid uuid NOT NULL, \
        \    format_uuid uuid NOT NULL, \
        \    \"position\" integer NOT NULL, \
        \    name character varying NOT NULL, \
        \    options jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template_locale ( \
        \    uuid uuid NOT NULL, \
        \    name varchar NOT NULL, \
        \    code varchar NOT NULL, \
        \    document_template_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE external_link_usage ( \
        \    uuid uuid NOT NULL, \
        \    url character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE instance_config_mail ( \
        \    uuid uuid NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    sender_name text, \
        \    sender_email text NOT NULL, \
        \    smtp_host text, \
        \    smtp_port integer, \
        \    smtp_security text, \
        \    smtp_username text, \
        \    smtp_password text, \
        \    rate_limit_window integer, \
        \    rate_limit_count integer, \
        \    timeout integer, \
        \    provider text DEFAULT 'smtp'::text NOT NULL, \
        \    aws_access_key_id text, \
        \    aws_secret_access_key text, \
        \    aws_region text \
        \); \
        \CREATE TABLE knowledge_model_cache ( \
        \    package_uuid uuid NOT NULL, \
        \    tag_uuids text[] NOT NULL, \
        \    knowledge_model jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_editor_event ( \
        \    uuid uuid NOT NULL, \
        \    parent_uuid uuid NOT NULL, \
        \    entity_uuid uuid NOT NULL, \
        \    content jsonb NOT NULL, \
        \    editor_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_editor_reply ( \
        \    path text NOT NULL, \
        \    value_type knowledge_model_editor_reply_type NOT NULL, \
        \    value text[], \
        \    value_raw jsonb, \
        \    editor_uuid uuid NOT NULL, \
        \    created_by jsonb, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_locale ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    code character varying NOT NULL, \
        \    knowledge_model_package_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_package_event ( \
        \    uuid uuid NOT NULL, \
        \    parent_uuid uuid NOT NULL, \
        \    entity_uuid uuid NOT NULL, \
        \    content jsonb NOT NULL, \
        \    package_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_secret ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    value character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE locale ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    code character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    locale_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    default_locale boolean NOT NULL, \
        \    license character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    recommended_app_version character varying NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE openid_client ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    url character varying NOT NULL, \
        \    client_id character varying NOT NULL, \
        \    client_secret character varying NOT NULL, \
        \    parameters jsonb NOT NULL, \
        \    style jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    registration_enabled boolean NOT NULL, \
        \    scope_profile boolean NOT NULL, \
        \    scope_email boolean NOT NULL \
        \); \
        \CREATE TABLE openid_client_session ( \
        \    state character varying NOT NULL, \
        \    nonce character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE persistent_command ( \
        \    uuid uuid NOT NULL, \
        \    state character varying NOT NULL, \
        \    component character varying NOT NULL, \
        \    function character varying NOT NULL, \
        \    body character varying NOT NULL, \
        \    last_error_message character varying, \
        \    attempts integer NOT NULL, \
        \    max_attempts integer NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    last_trace_uuid uuid \
        \); \
        \CREATE TABLE plugin ( \
        \    uuid uuid NOT NULL, \
        \    url character varying NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE prefab ( \
        \    uuid uuid NOT NULL, \
        \    type character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    content jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE project ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    visibility character varying NOT NULL, \
        \    sharing character varying NOT NULL, \
        \    knowledge_model_package_uuid uuid NOT NULL, \
        \    selected_question_tag_uuids uuid[] NOT NULL, \
        \    document_template_uuid uuid, \
        \    format_uuid uuid, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    description character varying, \
        \    is_template boolean NOT NULL, \
        \    squashed boolean NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    project_tags text[] NOT NULL, \
        \    language character varying, \
        \    document_template_language varchar \
        \); \
        \CREATE TABLE project_cache ( \
        \    project_uuid uuid NOT NULL, \
        \    questionnaire jsonb NOT NULL, \
        \    report jsonb NOT NULL, \
        \    versions jsonb NOT NULL, \
        \    questionnaire_source_updated_at timestamp with time zone NOT NULL, \
        \    versions_source_updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE project_comment ( \
        \    uuid uuid NOT NULL, \
        \    text text NOT NULL, \
        \    comment_thread_uuid uuid, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE project_comment_thread ( \
        \    uuid uuid NOT NULL, \
        \    path text NOT NULL, \
        \    resolved boolean NOT NULL, \
        \    private boolean NOT NULL, \
        \    project_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    assigned_to uuid, \
        \    assigned_by uuid, \
        \    notification_required boolean DEFAULT false NOT NULL \
        \); \
        \CREATE TABLE project_event ( \
        \    uuid uuid NOT NULL, \
        \    event_type project_event_type NOT NULL, \
        \    path text, \
        \    created_at timestamp with time zone NOT NULL, \
        \    created_by uuid, \
        \    project_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    value_type value_type, \
        \    value text[], \
        \    value_raw jsonb \
        \); \
        \CREATE TABLE project_file ( \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content_type character varying NOT NULL, \
        \    file_size bigint NOT NULL, \
        \    project_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE project_perm_group ( \
        \    project_uuid uuid NOT NULL, \
        \    user_group_uuid uuid NOT NULL, \
        \    perms text[] NOT NULL, \
        \    tenant_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE project_perm_user ( \
        \    project_uuid uuid NOT NULL, \
        \    user_uuid uuid NOT NULL, \
        \    perms text[] NOT NULL, \
        \    tenant_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE project_version ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying, \
        \    event_uuid uuid NOT NULL, \
        \    project_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE registry_document_template ( \
        \    organization_id character varying NOT NULL, \
        \    template_id character varying NOT NULL, \
        \    remote_version character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE registry_knowledge_model_package ( \
        \    organization_id character varying NOT NULL, \
        \    km_id character varying NOT NULL, \
        \    remote_version character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE registry_locale ( \
        \    organization_id character varying NOT NULL, \
        \    locale_id character varying NOT NULL, \
        \    remote_version character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE registry_organization ( \
        \    organization_id character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    logo character varying, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE role ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    permissions character varying[] NOT NULL, \
        \    is_admin boolean NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE submission ( \
        \    uuid uuid NOT NULL, \
        \    state character varying NOT NULL, \
        \    location character varying, \
        \    returned_data character varying, \
        \    service_id character varying NOT NULL, \
        \    document_uuid uuid, \
        \    created_by uuid, \
        \    created_at timestamp with time zone, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE temporary_file ( \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content_type character varying NOT NULL, \
        \    expires_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_by uuid, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE tenant ( \
        \    uuid uuid NOT NULL, \
        \    tenant_id character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    server_domain character varying NOT NULL, \
        \    client_url character varying NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    server_url character varying NOT NULL, \
        \    state character varying DEFAULT 'ReadyForUseTenantState'::character varying NOT NULL \
        \); \
        \CREATE TABLE tenant_limit_bundle ( \
        \    uuid uuid NOT NULL, \
        \    users integer NOT NULL, \
        \    active_users integer NOT NULL, \
        \    knowledge_models integer NOT NULL, \
        \    knowledge_model_editors integer NOT NULL, \
        \    document_templates integer NOT NULL, \
        \    projects integer NOT NULL, \
        \    documents integer NOT NULL, \
        \    storage bigint NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    document_template_drafts integer NOT NULL, \
        \    locales integer NOT NULL \
        \); \
        \CREATE TABLE tenant_module ( \
        \    tenant_uuid uuid NOT NULL, \
        \    \"position\" integer NOT NULL, \
        \    module_key character varying NOT NULL, \
        \    title character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    icon character varying NOT NULL, \
        \    url character varying NOT NULL, \
        \    external boolean NOT NULL, \
        \    required_permission character varying, \
        \    enabled boolean DEFAULT true NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE tenant_plugin_settings ( \
        \    tenant_uuid uuid NOT NULL, \
        \    plugin_uuid uuid NOT NULL, \
        \    \"values\" jsonb NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_email_link ( \
        \    uuid uuid NOT NULL, \
        \    identity uuid NOT NULL, \
        \    type character varying NOT NULL, \
        \    hash character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL \
        \); \
        \CREATE TABLE user_entity ( \
        \    uuid uuid NOT NULL, \
        \    first_name character varying NOT NULL, \
        \    last_name character varying NOT NULL, \
        \    email character varying NOT NULL, \
        \    password_hash character varying NOT NULL, \
        \    affiliation character varying, \
        \    role_uuid uuid NOT NULL, \
        \    role_permissions text[] NOT NULL, \
        \    active boolean NOT NULL, \
        \    image_url character varying, \
        \    last_visited_at timestamp with time zone NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    machine boolean NOT NULL, \
        \    locale uuid, \
        \    last_seen_news_id character varying, \
        \    email_verified_at timestamp with time zone, \
        \    email_pending character varying, \
        \    role_name character varying NOT NULL, \
        \    CONSTRAINT user_email_lowercase_check CHECK (((email)::text = lower((email)::text))) \
        \); \
        \CREATE TABLE user_entity_submission_prop ( \
        \    user_uuid uuid NOT NULL, \
        \    service_id character varying NOT NULL, \
        \    \"values\" jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_group ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying, \
        \    private boolean NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_group_membership ( \
        \    user_group_uuid uuid NOT NULL, \
        \    user_uuid uuid NOT NULL, \
        \    type character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_openid_identity ( \
        \    uuid uuid NOT NULL, \
        \    external_id character varying NOT NULL, \
        \    external_label character varying, \
        \    user_uuid uuid NOT NULL, \
        \    provider_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_plugin_settings ( \
        \    user_uuid uuid NOT NULL, \
        \    plugin_uuid uuid NOT NULL, \
        \    \"values\" jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_registration_pending ( \
        \    uuid uuid NOT NULL, \
        \    hash character varying NOT NULL, \
        \    service_type character varying NOT NULL, \
        \    provider_uuid uuid NOT NULL, \
        \    external_id character varying NOT NULL, \
        \    external_label character varying, \
        \    email character varying, \
        \    first_name character varying, \
        \    last_name character varying, \
        \    image_url character varying, \
        \    affiliation character varying, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_token ( \
        \    uuid uuid NOT NULL, \
        \    user_uuid uuid NOT NULL, \
        \    value character varying NOT NULL, \
        \    session_state character varying, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    name character varying NOT NULL, \
        \    type character varying NOT NULL, \
        \    user_agent character varying NOT NULL, \
        \    expires_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE user_tour ( \
        \    user_uuid uuid NOT NULL, \
        \    tour_id character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createConstraints :: Pool Connection -> LoggingT IO ()
createConstraints dbPool = do
  let sql =
        "ALTER TABLE ONLY audit \
        \    ADD CONSTRAINT audit_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY component \
        \    ADD CONSTRAINT component_pk PRIMARY KEY (name); \
        \ALTER TABLE ONLY config_authentication \
        \    ADD CONSTRAINT config_authentication_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_dashboard_and_login_screen_announcement \
        \    ADD CONSTRAINT config_dashboard_and_login_screen_announcement_pk PRIMARY KEY (tenant_uuid, \"position\"); \
        \ALTER TABLE ONLY config_dashboard_and_login_screen \
        \    ADD CONSTRAINT config_dashboard_and_login_screen_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_features \
        \    ADD CONSTRAINT config_features_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_look_and_feel_custom_menu_link \
        \    ADD CONSTRAINT config_look_and_feel_custom_menu_link_pk PRIMARY KEY (tenant_uuid, \"position\"); \
        \ALTER TABLE ONLY config_look_and_feel \
        \    ADD CONSTRAINT config_look_and_feel_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_mail \
        \    ADD CONSTRAINT config_mail_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_organization \
        \    ADD CONSTRAINT config_organization_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_owl \
        \    ADD CONSTRAINT config_owl_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_privacy_and_support \
        \    ADD CONSTRAINT config_privacy_and_support_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_project \
        \    ADD CONSTRAINT config_project_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_registry \
        \    ADD CONSTRAINT config_registry_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_submission \
        \    ADD CONSTRAINT config_submission_pk PRIMARY KEY (tenant_uuid); \
        \ALTER TABLE ONLY config_submission_service \
        \    ADD CONSTRAINT config_submission_service_pk PRIMARY KEY (tenant_uuid, id); \
        \ALTER TABLE ONLY config_submission_service_request_header \
        \    ADD CONSTRAINT config_submission_service_request_header_pk PRIMARY KEY (tenant_uuid, service_id, name); \
        \ALTER TABLE ONLY config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_pk PRIMARY KEY (tenant_uuid, service_id, document_template_uuid, format_uuid); \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template_asset \
        \    ADD CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template_draft_data \
        \    ADD CONSTRAINT document_template_draft_data_pk PRIMARY KEY (document_template_uuid); \
        \ALTER TABLE ONLY document_template_file \
        \    ADD CONSTRAINT document_template_file_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template_format \
        \    ADD CONSTRAINT document_template_format_pk PRIMARY KEY (uuid, document_template_uuid); \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_uuid, format_uuid, \"position\"); \
        \ALTER TABLE ONLY document_template_locale \
        \    ADD CONSTRAINT document_template_locale_code_unique UNIQUE (document_template_uuid, code, tenant_uuid); \
        \ALTER TABLE ONLY document_template_locale \
        \    ADD CONSTRAINT document_template_locale_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template \
        \    ADD CONSTRAINT document_template_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY external_link_usage \
        \    ADD CONSTRAINT external_link_usage_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY instance_config_mail \
        \    ADD CONSTRAINT instance_config_mail_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_cache \
        \    ADD CONSTRAINT knowledge_model_cache_pk PRIMARY KEY (package_uuid, tag_uuids); \
        \ALTER TABLE ONLY knowledge_model_editor_event \
        \    ADD CONSTRAINT knowledge_model_editor_event_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_editor \
        \    ADD CONSTRAINT knowledge_model_editor_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_editor_reply \
        \    ADD CONSTRAINT knowledge_model_editor_reply_pk PRIMARY KEY (editor_uuid, path); \
        \ALTER TABLE ONLY knowledge_model_locale \
        \    ADD CONSTRAINT knowledge_model_locale_code_unique UNIQUE (knowledge_model_package_uuid, code, tenant_uuid); \
        \ALTER TABLE ONLY knowledge_model_locale \
        \    ADD CONSTRAINT knowledge_model_locale_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_migration \
        \    ADD CONSTRAINT knowledge_model_migration_pk PRIMARY KEY (editor_uuid); \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_coordinate_unique UNIQUE (organization_id, km_id, version, tenant_uuid); \
        \ALTER TABLE ONLY knowledge_model_package_event \
        \    ADD CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_uuid); \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_secret \
        \    ADD CONSTRAINT knowledge_model_secret_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY locale \
        \    ADD CONSTRAINT locale_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY openid_client \
        \    ADD CONSTRAINT openid_client_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY openid_client_session \
        \    ADD CONSTRAINT openid_client_session_pk PRIMARY KEY (state); \
        \ALTER TABLE ONLY persistent_command \
        \    ADD CONSTRAINT persistent_command_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY plugin \
        \    ADD CONSTRAINT plugin_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE ONLY prefab \
        \    ADD CONSTRAINT prefab_pk PRIMARY KEY (uuid, tenant_uuid); \
        \ALTER TABLE ONLY project_cache \
        \    ADD CONSTRAINT project_cache_pk PRIMARY KEY (project_uuid); \
        \ALTER TABLE ONLY project_comment \
        \    ADD CONSTRAINT project_comment_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY project_comment_thread \
        \    ADD CONSTRAINT project_comment_thread_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY project_event \
        \    ADD CONSTRAINT project_event_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY project_file \
        \    ADD CONSTRAINT project_file_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY project_perm_group \
        \    ADD CONSTRAINT project_perm_group_pk PRIMARY KEY (user_group_uuid, project_uuid); \
        \ALTER TABLE ONLY project_perm_user \
        \    ADD CONSTRAINT project_perm_user_pk PRIMARY KEY (user_uuid, project_uuid); \
        \ALTER TABLE ONLY project \
        \    ADD CONSTRAINT project_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY project_version \
        \    ADD CONSTRAINT project_version_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY registry_document_template \
        \    ADD CONSTRAINT registry_document_template_pk PRIMARY KEY (organization_id, template_id); \
        \ALTER TABLE ONLY registry_knowledge_model_package \
        \    ADD CONSTRAINT registry_knowledge_model_package_pk PRIMARY KEY (organization_id, km_id); \
        \ALTER TABLE ONLY registry_locale \
        \    ADD CONSTRAINT registry_locale_pk PRIMARY KEY (organization_id, locale_id); \
        \ALTER TABLE ONLY registry_organization \
        \    ADD CONSTRAINT registry_organization_pk PRIMARY KEY (organization_id); \
        \ALTER TABLE ONLY role \
        \    ADD CONSTRAINT role_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY submission \
        \    ADD CONSTRAINT submission_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY temporary_file \
        \    ADD CONSTRAINT temporary_file_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY tenant_limit_bundle \
        \    ADD CONSTRAINT tenant_limit_bundle_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY tenant_module \
        \    ADD CONSTRAINT tenant_module_pk PRIMARY KEY (tenant_uuid, \"position\"); \
        \ALTER TABLE ONLY tenant \
        \    ADD CONSTRAINT tenant_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY tenant_plugin_settings \
        \    ADD CONSTRAINT tenant_plugin_settings_pk PRIMARY KEY (tenant_uuid, plugin_uuid); \
        \ALTER TABLE ONLY user_email_link \
        \    ADD CONSTRAINT user_email_link_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_entity \
        \    ADD CONSTRAINT user_entity_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_entity_submission_prop \
        \    ADD CONSTRAINT user_entity_submission_prop_pk PRIMARY KEY (user_uuid, service_id); \
        \ALTER TABLE ONLY user_group_membership \
        \    ADD CONSTRAINT user_group_membership_pk PRIMARY KEY (user_group_uuid, user_uuid); \
        \ALTER TABLE ONLY user_group \
        \    ADD CONSTRAINT user_group_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_openid_identity \
        \    ADD CONSTRAINT user_openid_identity_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_plugin_settings \
        \    ADD CONSTRAINT user_plugin_settings_pk PRIMARY KEY (user_uuid, plugin_uuid); \
        \ALTER TABLE ONLY user_registration_pending \
        \    ADD CONSTRAINT user_registration_pending_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_token \
        \    ADD CONSTRAINT user_token_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_tour \
        \    ADD CONSTRAINT user_tour_pk PRIMARY KEY (user_uuid, tour_id);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createIndexes :: Pool Connection -> LoggingT IO ()
createIndexes dbPool = do
  let sql =
        "CREATE INDEX persistent_command_queue_idx ON persistent_command USING btree (component, created_at) WHERE state <> 'DonePersistentCommandState'; \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template USING btree (organization_id, template_id, tenant_uuid); \
        \CREATE INDEX knowledge_model_package_organization_id_km_id_index ON knowledge_model_package USING btree (organization_id, km_id, tenant_uuid); \
        \CREATE INDEX knowledge_model_package_previous_package_uuid_index ON knowledge_model_package USING btree (previous_package_uuid); \
        \CREATE INDEX project_cache_tenant_uuid_index ON project_cache USING btree (tenant_uuid); \
        \CREATE INDEX submission_document_uuid_index ON submission USING btree (document_uuid, tenant_uuid); \
        \CREATE UNIQUE INDEX user_email_link_hash_uindex ON user_email_link USING btree (hash); \
        \CREATE UNIQUE INDEX user_email_uindex ON user_entity USING btree (email, tenant_uuid); \
        \CREATE INDEX user_group_membership_user_uuid_idx ON user_group_membership USING btree (user_uuid, tenant_uuid); \
        \CREATE UNIQUE INDEX user_openid_identity_uindex ON user_openid_identity USING btree (external_id, provider_uuid, tenant_uuid); \
        \CREATE INDEX user_openid_identity_user_uuid_idx ON user_openid_identity USING btree (user_uuid, tenant_uuid); \
        \CREATE UNIQUE INDEX user_registration_pending_hash_uindex ON user_registration_pending USING btree (hash, tenant_uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTriggers :: Pool Connection -> LoggingT IO ()
createTriggers dbPool = do
  let sql =
        "CREATE TRIGGER trg_knowledge_model_locale_after_delete_s3 AFTER DELETE ON knowledge_model_locale FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_entity_uuid('knowledge_model_locale', 'deleteFromS3'); \
        \CREATE TRIGGER trg_locale_after_delete_s3 AFTER DELETE ON locale FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_entity_uuid('locale', 'deleteFromS3'); \
        \CREATE TRIGGER trigger_on_after_document_delete AFTER DELETE ON document FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_document_delete(); \
        \CREATE TRIGGER trigger_on_after_document_template_locale_delete AFTER DELETE ON document_template_locale FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_entity_uuid('document_template_locale', 'deleteFromS3'); \
        \CREATE TRIGGER trigger_on_after_document_template_asset_delete AFTER DELETE ON document_template_asset FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_document_template_asset_delete(); \
        \CREATE TRIGGER trigger_on_after_project_file_delete AFTER DELETE ON project_file FOR EACH ROW EXECUTE FUNCTION create_persistent_command_from_project_file_delete();"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createForeignKeys :: Pool Connection -> LoggingT IO ()
createForeignKeys dbPool = do
  let sql =
        "ALTER TABLE ONLY audit \
        \    ADD CONSTRAINT audit_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY audit \
        \    ADD CONSTRAINT audit_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_authentication \
        \    ADD CONSTRAINT config_authentication_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_dashboard_and_login_screen_announcement \
        \    ADD CONSTRAINT config_dashboard_and_login_screen_announcement_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_dashboard_and_login_screen \
        \    ADD CONSTRAINT config_dashboard_and_login_screen_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_features \
        \    ADD CONSTRAINT config_features_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_look_and_feel_custom_menu_link \
        \    ADD CONSTRAINT config_look_and_feel_custom_menu_link_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_look_and_feel \
        \    ADD CONSTRAINT config_look_and_feel_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_mail \
        \    ADD CONSTRAINT config_mail_config_uuid_fk FOREIGN KEY (config_uuid) REFERENCES instance_config_mail(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY config_mail \
        \    ADD CONSTRAINT config_mail_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_organization \
        \    ADD CONSTRAINT config_organization_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_owl \
        \    ADD CONSTRAINT config_owl_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_privacy_and_support \
        \    ADD CONSTRAINT config_privacy_and_support_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_project \
        \    ADD CONSTRAINT config_project_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_registry \
        \    ADD CONSTRAINT config_registry_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_request_header \
        \    ADD CONSTRAINT config_submission_service_request_header_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service(id, tenant_uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_request_header \
        \    ADD CONSTRAINT config_submission_service_request_header_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_document_template_ FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format(document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_service_id_fk FOREIGN KEY (service_id, tenant_uuid) REFERENCES config_submission_service(id, tenant_uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service_supported_format \
        \    ADD CONSTRAINT config_submission_service_supported_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission_service \
        \    ADD CONSTRAINT config_submission_service_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY config_submission \
        \    ADD CONSTRAINT config_submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format(document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_asset \
        \    ADD CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_asset \
        \    ADD CONSTRAINT document_template_asset_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_draft_data \
        \    ADD CONSTRAINT document_template_draft_data_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_draft_data \
        \    ADD CONSTRAINT document_template_draft_data_knowledge_model_editor_uuid_fk FOREIGN KEY (knowledge_model_editor_uuid) REFERENCES knowledge_model_editor(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_draft_data \
        \    ADD CONSTRAINT document_template_draft_data_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY document_template_draft_data \
        \    ADD CONSTRAINT document_template_draft_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_file \
        \    ADD CONSTRAINT document_template_file_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_file \
        \    ADD CONSTRAINT document_template_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format \
        \    ADD CONSTRAINT document_template_format_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format(document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format \
        \    ADD CONSTRAINT document_template_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_locale \
        \    ADD CONSTRAINT document_template_locale_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_locale \
        \    ADD CONSTRAINT document_template_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template \
        \    ADD CONSTRAINT document_template_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document \
        \    ADD CONSTRAINT document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY external_link_usage \
        \    ADD CONSTRAINT external_link_usage_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_cache \
        \    ADD CONSTRAINT knowledge_model_cache_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_cache \
        \    ADD CONSTRAINT knowledge_model_cache_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor \
        \    ADD CONSTRAINT knowledge_model_editor_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY knowledge_model_editor_event \
        \    ADD CONSTRAINT knowledge_model_editor_event_editor_uuid_fk FOREIGN KEY (editor_uuid) REFERENCES knowledge_model_editor(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor_event \
        \    ADD CONSTRAINT knowledge_model_editor_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor \
        \    ADD CONSTRAINT knowledge_model_editor_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor_reply \
        \    ADD CONSTRAINT knowledge_model_editor_reply_editor_uuid FOREIGN KEY (editor_uuid) REFERENCES knowledge_model_editor(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor_reply \
        \    ADD CONSTRAINT knowledge_model_editor_reply_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_editor \
        \    ADD CONSTRAINT knowledge_model_editor_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_locale \
        \    ADD CONSTRAINT knowledge_model_locale_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_locale \
        \    ADD CONSTRAINT knowledge_model_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_migration \
        \    ADD CONSTRAINT knowledge_model_migration_editor_previous_package_uuid_fk FOREIGN KEY (editor_previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_migration \
        \    ADD CONSTRAINT knowledge_model_migration_editor_uuid_fk FOREIGN KEY (editor_uuid) REFERENCES knowledge_model_editor(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_migration \
        \    ADD CONSTRAINT knowledge_model_migration_target_package_uuid_fk FOREIGN KEY (target_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_migration \
        \    ADD CONSTRAINT knowledge_model_migration_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package_event \
        \    ADD CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package_event \
        \    ADD CONSTRAINT knowledge_model_package_event_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_secret \
        \    ADD CONSTRAINT knowledge_model_secret_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY locale \
        \    ADD CONSTRAINT locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY openid_client_session \
        \    ADD CONSTRAINT openid_client_session_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY openid_client \
        \    ADD CONSTRAINT openid_client_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY persistent_command \
        \    ADD CONSTRAINT persistent_command_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY persistent_command \
        \    ADD CONSTRAINT persistent_command_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY prefab \
        \    ADD CONSTRAINT prefab_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_cache \
        \    ADD CONSTRAINT project_cache_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_cache \
        \    ADD CONSTRAINT project_cache_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_comment \
        \    ADD CONSTRAINT project_comment_comment_thread_uuid FOREIGN KEY (comment_thread_uuid) REFERENCES project_comment_thread(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_comment \
        \    ADD CONSTRAINT project_comment_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_comment_thread \
        \    ADD CONSTRAINT project_comment_thread_assigned_by FOREIGN KEY (assigned_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project_comment_thread \
        \    ADD CONSTRAINT project_comment_thread_assigned_to FOREIGN KEY (assigned_to) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project_comment_thread \
        \    ADD CONSTRAINT project_comment_thread_project_uuid FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_comment_thread \
        \    ADD CONSTRAINT project_comment_thread_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project \
        \    ADD CONSTRAINT project_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project \
        \    ADD CONSTRAINT project_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_event \
        \    ADD CONSTRAINT project_event_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project_event \
        \    ADD CONSTRAINT project_event_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_event \
        \    ADD CONSTRAINT project_event_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_file \
        \    ADD CONSTRAINT project_file_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project_file \
        \    ADD CONSTRAINT project_file_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_file \
        \    ADD CONSTRAINT project_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project \
        \    ADD CONSTRAINT project_knowledge_model_package_uuid_fk FOREIGN KEY (knowledge_model_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_group \
        \    ADD CONSTRAINT project_perm_group_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_group \
        \    ADD CONSTRAINT project_perm_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_group \
        \    ADD CONSTRAINT project_perm_group_user_group_uuid_fk FOREIGN KEY (user_group_uuid) REFERENCES user_group(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_user \
        \    ADD CONSTRAINT project_perm_user_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_user \
        \    ADD CONSTRAINT project_perm_user_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_perm_user \
        \    ADD CONSTRAINT project_perm_user_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project \
        \    ADD CONSTRAINT project_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_version \
        \    ADD CONSTRAINT project_version_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY project_version \
        \    ADD CONSTRAINT project_version_event_uuid_fk FOREIGN KEY (event_uuid) REFERENCES project_event(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_version \
        \    ADD CONSTRAINT project_version_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY project_version \
        \    ADD CONSTRAINT project_version_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY role \
        \    ADD CONSTRAINT role_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY submission \
        \    ADD CONSTRAINT submission_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY submission \
        \    ADD CONSTRAINT submission_document_uuid_fk FOREIGN KEY (document_uuid) REFERENCES document(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY submission \
        \    ADD CONSTRAINT submission_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service(tenant_uuid, id) ON DELETE CASCADE; \
        \ALTER TABLE ONLY submission \
        \    ADD CONSTRAINT submission_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY temporary_file \
        \    ADD CONSTRAINT temporary_file_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY temporary_file \
        \    ADD CONSTRAINT temporary_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY tenant_limit_bundle \
        \    ADD CONSTRAINT tenant_limit_bundle_uuid_fk FOREIGN KEY (uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY tenant_module \
        \    ADD CONSTRAINT tenant_module_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY tenant_plugin_settings \
        \    ADD CONSTRAINT tenant_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin(uuid, tenant_uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY tenant_plugin_settings \
        \    ADD CONSTRAINT tenant_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_email_link \
        \    ADD CONSTRAINT user_email_link_identity_fk FOREIGN KEY (identity) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_email_link \
        \    ADD CONSTRAINT user_email_link_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_entity \
        \    ADD CONSTRAINT user_entity_locale_fk FOREIGN KEY (locale) REFERENCES locale(uuid) ON DELETE SET NULL; \
        \ALTER TABLE ONLY user_entity \
        \    ADD CONSTRAINT user_entity_role_uuid_fk FOREIGN KEY (role_uuid) REFERENCES role(uuid); \
        \ALTER TABLE ONLY user_entity_submission_prop \
        \    ADD CONSTRAINT user_entity_submission_prop_service_id_fk FOREIGN KEY (tenant_uuid, service_id) REFERENCES config_submission_service(tenant_uuid, id) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_entity_submission_prop \
        \    ADD CONSTRAINT user_entity_submission_prop_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_entity_submission_prop \
        \    ADD CONSTRAINT user_entity_submission_prop_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_entity \
        \    ADD CONSTRAINT user_entity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_group_membership \
        \    ADD CONSTRAINT user_group_membership_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_group_membership \
        \    ADD CONSTRAINT user_group_membership_user_group_uuid_fk FOREIGN KEY (user_group_uuid) REFERENCES user_group(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_group_membership \
        \    ADD CONSTRAINT user_group_membership_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_group \
        \    ADD CONSTRAINT user_group_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_openid_identity \
        \    ADD CONSTRAINT user_openid_identity_provider_uuid_fk FOREIGN KEY (provider_uuid) REFERENCES openid_client(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_openid_identity \
        \    ADD CONSTRAINT user_openid_identity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_openid_identity \
        \    ADD CONSTRAINT user_openid_identity_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_plugin_settings \
        \    ADD CONSTRAINT user_plugin_settings_plugin_uuid_fk FOREIGN KEY (plugin_uuid, tenant_uuid) REFERENCES plugin(uuid, tenant_uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_plugin_settings \
        \    ADD CONSTRAINT user_plugin_settings_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_plugin_settings \
        \    ADD CONSTRAINT user_plugin_settings_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_registration_pending \
        \    ADD CONSTRAINT user_registration_pending_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_token \
        \    ADD CONSTRAINT user_token_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_token \
        \    ADD CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_tour \
        \    ADD CONSTRAINT user_tour_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY user_tour \
        \    ADD CONSTRAINT user_tour_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity(uuid) ON DELETE CASCADE;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

insertTenant :: Pool Connection -> LoggingT IO ()
insertTenant dbPool = do
  let sql =
        "INSERT INTO tenant (uuid, tenant_id, name, server_domain, server_url, client_url, enabled, state, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', \
        \        'default', \
        \        'Default Tenant', \
        \        'server.example.com', \
        \        'https://server.example.com', \
        \        'client.example.com', \
        \        true, \
        \        'ReadyForUseTenantState', \
        \        '2021-10-18 08:25:17.016000 +00:00', \
        \        '2021-10-18 08:25:18.326000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

insertRoles :: Pool Connection -> LoggingT IO ()
insertRoles dbPool = do
  let sql =
        "INSERT INTO role (uuid, name, permissions, is_admin, tenant_uuid, created_at, updated_at) \
        \VALUES ('a0000000-0000-0000-0000-000000000001', 'Admin', '{UsersManageRolePermission,SettingsManageRolePermission,ProjectTemplatesManageRolePermission,ProjectsViewRolePermission,ProjectsCommentRolePermission,ProjectsEditRolePermission,ProjectsManageRolePermission,KnowledgeModelEditorsUseRolePermission,KnowledgeModelsManageRolePermission,DocumentTemplateEditorsUseRolePermission,DocumentTemplatesManageRolePermission}', true, '00000000-0000-0000-0000-000000000000', now(), now()), \
        \       ('a0000000-0000-0000-0000-000000000002', 'Data Steward', '{ProjectTemplatesManageRolePermission,KnowledgeModelEditorsUseRolePermission,KnowledgeModelsManageRolePermission,DocumentTemplateEditorsUseRolePermission,DocumentTemplatesManageRolePermission}', false, '00000000-0000-0000-0000-000000000000', now(), now()), \
        \       ('a0000000-0000-0000-0000-000000000003', 'Researcher', '{}', false, '00000000-0000-0000-0000-000000000000', now(), now());"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

-- cspell: disable
insertUsers :: Pool Connection -> LoggingT IO ()
insertUsers dbPool = do
  let sql =
        "INSERT INTO user_entity (uuid, first_name, last_name, email, password_hash, affiliation, role_uuid, \
        \                          role_name, role_permissions, active, image_url, machine, tenant_uuid, locale, \
        \                          last_seen_news_id, email_verified_at, email_pending, last_visited_at, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', 'System', 'User', 'system@example.com', 'no-hash', null, 'a0000000-0000-0000-0000-000000000001', 'Admin', '{UsersManageRolePermission,SettingsManageRolePermission,ProjectTemplatesManageRolePermission,ProjectsViewRolePermission,ProjectsCommentRolePermission,ProjectsEditRolePermission,ProjectsManageRolePermission,KnowledgeModelEditorsUseRolePermission,KnowledgeModelsManageRolePermission,DocumentTemplateEditorsUseRolePermission,DocumentTemplatesManageRolePermission,TenantsManageRolePermission,DevUseRolePermission}', \
        \        true, null, true, '00000000-0000-0000-0000-000000000000', null, null, '2018-01-20 00:00:00.000000 +00:00', null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00', '2018-01-25 00:00:00.000000 +00:00'), \
        \       ('ec6f8e90-2a91-49ec-aa3f-9eab2267fc66', 'Albert', 'Einstein', 'albert.einstein@example.com', 'pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        'My University', 'a0000000-0000-0000-0000-000000000001', 'Admin', '{UsersManageRolePermission,SettingsManageRolePermission,ProjectTemplatesManageRolePermission,ProjectsViewRolePermission,ProjectsCommentRolePermission,ProjectsEditRolePermission,ProjectsManageRolePermission,KnowledgeModelEditorsUseRolePermission,KnowledgeModelsManageRolePermission,DocumentTemplateEditorsUseRolePermission,DocumentTemplatesManageRolePermission}', \
        \        true, null, false, '00000000-0000-0000-0000-000000000000', null, null, '2018-01-20 00:00:00.000000 +00:00', null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00', '2018-01-25 00:00:00.000000 +00:00'), \
        \       ('30d48cf4-8c8a-496f-bafe-585bd238f798', 'Nikola', 'Tesla', 'nikola.tesla@example.com', 'pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', null, 'a0000000-0000-0000-0000-000000000002', \
        \        'Data Steward', '{ProjectTemplatesManageRolePermission,KnowledgeModelEditorsUseRolePermission,KnowledgeModelsManageRolePermission,DocumentTemplateEditorsUseRolePermission,DocumentTemplatesManageRolePermission}', \
        \        true, null, false, '00000000-0000-0000-0000-000000000000', null, null, '2018-01-20 00:00:00.000000 +00:00', null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00', '2018-01-25 00:00:00.000000 +00:00'), \
        \       ('e1c58e52-0824-4526-8ebe-ec38eec67030', 'Isaac', 'Newton', 'isaac.newton@example.com', 'pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', null, 'a0000000-0000-0000-0000-000000000003', \
        \        'Researcher', '{}', \
        \        true, null, false, '00000000-0000-0000-0000-000000000000', null, null, '2018-01-20 00:00:00.000000 +00:00', null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00', '2018-01-25 00:00:00.000000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

-- cspell: enable

insertTenantLimitBundle :: Pool Connection -> LoggingT IO ()
insertTenantLimitBundle dbPool = do
  let sql =
        "INSERT INTO tenant_limit_bundle (uuid, users, active_users, knowledge_models, knowledge_model_editors, \
        \                                    document_templates, document_template_drafts, projects, documents, locales, \
        \                                    storage, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', -30000, -30000, -60000, -60000, -60000, -60000, -60000, -180000, -60000, -1500000000, \
        \        '2021-10-18 08:25:17.016000 +00:00', \
        \        '2021-10-18 08:25:18.326000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

insertLocale :: Pool Connection -> LoggingT IO ()
insertLocale dbPool = do
  let sql =
        "INSERT INTO locale (uuid, name, description, code, organization_id, locale_id, version, default_locale, \
        \                     license, readme, recommended_app_version, enabled, tenant_uuid, created_at, updated_at) \
        \VALUES ('7fb838c5-9279-4a78-8c2b-86ee9762a95f', \
        \        'English', \
        \        'Default English locale for Wizard UI', \
        \        'en', \
        \        '~', \
        \        'default', \
        \        '1.0.0', \
        \        true, \
        \        'Apache-2.0', \
        \        concat('# Default English Locale for Wizard Client', \
        \              CHR(13), CHR(10), CHR(13), CHR(10), \
        \              '[![Language](https://img.shields.io/badge/ISO%20639--1-en-blue)](https://en.wikipedia.org/wiki/English_language)', \
        \              CHR(13), CHR(10), CHR(13), CHR(10), \
        \              'This is the default English locale embedded in the Wizard Client. Therefore, it is always complete and compatible with the version that it is shipped with.', \
        \              CHR(13), CHR(10), CHR(13), CHR(10), \
        \              'The locale also cannot be exported or deleted. However, you can *Disable* it anytime as well as mark other locale to be used as *Default* if necessary.', \
        \              CHR(13), CHR(10), CHR(13), CHR(10), \
        \              'In case you encounter any issues with this issue, please contact your service provider.', \
        \              CHR(13), CHR(10)), \
        \        '3.18.0', \
        \        true, \
        \        '00000000-0000-0000-0000-000000000000', \
        \        '2022-01-21 00:00:00.000000 +00:00', \
        \        '2022-01-21 00:00:00.000000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

insertConfig :: Pool Connection -> LoggingT IO ()
insertConfig dbPool = do
  let sql =
        "INSERT INTO config_organization (tenant_uuid, name, description, organization_id, affiliations, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', 'My Organization', 'My Organization Description', 'myorg', '{}', '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_authentication (tenant_uuid, default_role_uuid, internal_registration_enabled, \
        \                                     internal_two_factor_auth_enabled, internal_two_factor_auth_code_length, \
        \                                     internal_two_factor_auth_code_expiration, internal_non_admin_login_enabled, \
        \                                     internal_session_expiration, internal_user_email_link_expiration, \
        \                                     created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', 'a0000000-0000-0000-0000-000000000003', true, false, 6, 600, true, 336, 336, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_privacy_and_support (tenant_uuid, privacy_url, terms_of_service_url, support_email, \
        \                                          support_site_name, support_site_url, support_site_icon, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', null, null, null, null, null, null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_dashboard_and_login_screen (tenant_uuid, dashboard_type, login_info, login_info_sidebar, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', 'RoleBasedDashboardType', null, null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_look_and_feel (tenant_uuid, app_title, app_title_short, logo_url, primary_color, \
        \                                    illustrations_color, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', null, null, null, null, null, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_registry (tenant_uuid, enabled, token, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', false, '', '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_project (tenant_uuid, visibility_enabled, visibility_default_value, sharing_enabled, \
        \                              sharing_default_value, sharing_anonymous_enabled, creation, project_tagging_enabled, \
        \                              project_tagging_tags, summary_report, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', true, 'PrivateProjectVisibility', true, 'RestrictedProjectSharing', false, \
        \        'TemplateAndCustomProjectCreation', true, '{}', true, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_submission (tenant_uuid, enabled, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', true, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_owl (tenant_uuid, enabled, name, organization_id, km_id, version, previous_package_uuid, \
        \                          root_element, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', false, '', '', '', '', null, '', '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_features (tenant_uuid, ai_assistant_enabled, tours_enabled, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', true, true, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00'); \
        \INSERT INTO config_mail (tenant_uuid, config_uuid, custom_templates, created_at, updated_at) \
        \VALUES ('00000000-0000-0000-0000-000000000000', null, false, '2018-01-20 00:00:00.000000 +00:00', '2018-01-20 00:00:00.000000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()
