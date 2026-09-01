module Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration where

import Control.Monad.Except (catchError)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.S3.Common
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) drop tables"
  let sql =
        "DROP TABLE IF EXISTS document_template_locale CASCADE;\
        \DROP TABLE IF EXISTS document_template_draft_data CASCADE;\
        \DROP TABLE IF EXISTS document_template_asset CASCADE;\
        \DROP TABLE IF EXISTS document_template_file CASCADE;\
        \DROP TABLE IF EXISTS document_template_format_step CASCADE;\
        \DROP TABLE IF EXISTS document_template_format CASCADE;\
        \DROP TABLE IF EXISTS document_template CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropBucket :: WizardRequestContextC s m => m ()
dropBucket = do
  catchError purgeBucket (\e -> return ())
  catchError removeBucket (\e -> return ())

dropFunctions :: WizardRequestContextC s m => m Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) drop functions"
  let sql = "DROP FUNCTION IF EXISTS create_persistent_command_from_project_file_delete;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: WizardRequestContextC s m => m Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/DocumentTemplate) drop tables"
  let sql =
        "DROP TRIGGER IF EXISTS trigger_on_after_document_template_asset_delete ON document_template_asset; \
        \DROP TRIGGER IF EXISTS trigger_on_after_document_template_locale_delete ON document_template_locale;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m ()
createTables = do
  createTemplateTable
  createTemplateFormatTable
  createTemplateFileTable
  createTemplateAssetTable
  createTemplateLocaleTable
  makeBucket
  makeBucketPublicReadOnly

createTemplateTable :: WizardRequestContextC s m => m Int64
createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplate) create table"
  let sql =
        "CREATE TABLE document_template \
        \( \
        \    uuid              uuid             NOT NULL, \
        \    name              varchar          NOT NULL, \
        \    organization_id   varchar          NOT NULL, \
        \    template_id       varchar          NOT NULL, \
        \    version           varchar          NOT NULL, \
        \    metamodel_version sem_ver_2_tuple  NOT NULL, \
        \    description       varchar          NOT NULL, \
        \    readme            varchar          NOT NULL, \
        \    license           varchar          NOT NULL, \
        \    allowed_packages  jsonb            NOT NULL, \
        \    created_at        timestamptz      NOT NULL, \
        \    tenant_uuid       uuid             NOT NULL, \
        \    updated_at        timestamptz      NOT NULL, \
        \    phase             varchar          NOT NULL, \
        \    non_editable      boolean          NOT NULL, \
        \    language          varchar          NOT NULL, \
        \    pot_file_ready    boolean          NOT NULL, \
        \    CONSTRAINT document_template_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template (organization_id, template_id, tenant_uuid);"
  let action conn = execute_ conn sql
  runDB action

createTemplateFormatTable :: WizardRequestContextC s m => m Int64
createTemplateFormatTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFormat) create table"
  let sql =
        "CREATE TABLE document_template_format \
        \( \
        \    document_template_uuid uuid        NOT NULL, \
        \    uuid                   uuid        NOT NULL, \
        \    name                   varchar     NOT NULL, \
        \    icon                   varchar     NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_pk PRIMARY KEY (uuid, document_template_uuid), \
        \    CONSTRAINT document_template_format_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE TABLE document_template_format_step \
        \( \
        \    document_template_uuid uuid        NOT NULL, \
        \    format_uuid            uuid        NOT NULL, \
        \    position               int         NOT NULL, \
        \    name                   varchar     NOT NULL, \
        \    options                jsonb       NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_uuid, format_uuid, position), \
        \    CONSTRAINT document_template_format_step_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format (document_template_uuid, uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_format_step_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateFileTable :: WizardRequestContextC s m => m Int64
createTemplateFileTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateFile) create table"
  let sql =
        "CREATE TABLE document_template_file \
        \( \
        \    document_template_uuid uuid        NOT NULL, \
        \    uuid                   uuid        NOT NULL, \
        \    file_name              varchar     NOT NULL, \
        \    content                varchar     NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    CONSTRAINT document_template_file_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_file_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_file_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateAssetTable :: WizardRequestContextC s m => m Int64
createTemplateAssetTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateAsset) create table"
  let sql =
        "CREATE TABLE document_template_asset \
        \( \
        \    document_template_uuid uuid        NOT NULL, \
        \    uuid                   uuid        NOT NULL, \
        \    file_name              varchar     NOT NULL, \
        \    content_type           varchar     NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    file_size              bigint      NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_asset_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateLocaleTable :: WizardRequestContextC s m => m Int64
createTemplateLocaleTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateLocale) create table"
  let sql =
        "CREATE TABLE document_template_locale \
        \( \
        \    uuid                   uuid        NOT NULL, \
        \    name                   varchar     NOT NULL, \
        \    code                   varchar     NOT NULL, \
        \    document_template_uuid uuid        NOT NULL, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    CONSTRAINT document_template_locale_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT document_template_locale_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_locale_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_locale_code_unique UNIQUE (document_template_uuid, code, tenant_uuid) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createDraftDataTable :: WizardRequestContextC s m => m Int64
createDraftDataTable = do
  logInfo _CMP_MIGRATION "(Table/DocumentTemplateDraftData) create table"
  let sql =
        "CREATE TABLE document_template_draft_data \
        \( \
        \    document_template_uuid uuid        NOT NULL, \
        \    project_uuid           uuid, \
        \    format_uuid            uuid, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    created_at             timestamptz NOT NULL, \
        \    updated_at             timestamptz NOT NULL, \
        \    knowledge_model_editor_uuid  uuid, \
        \    CONSTRAINT document_template_draft_data_pk PRIMARY KEY (document_template_uuid), \
        \    CONSTRAINT document_template_draft_data_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_draft_data_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES project (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT document_template_draft_data_knowledge_model_editor_uuid_fk FOREIGN KEY (knowledge_model_editor_uuid) REFERENCES knowledge_model_editor (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT document_template_draft_data_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: WizardRequestContextC s m => m Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/DocumentTemplate) create functions"
  createPersistentCommandFromDocumentTemplateAssetDeleteFunction

createPersistentCommandFromDocumentTemplateAssetDeleteFunction :: WizardRequestContextC s m => m Int64
createPersistentCommandFromDocumentTemplateAssetDeleteFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION create_persistent_command_from_document_template_asset_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM create_persistent_command( \
        \            'document_template_asset', \
        \            'deleteFromS3', \
        \            jsonb_build_object('documentTemplateUuid', OLD.document_template_uuid, 'assetUuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: WizardRequestContextC s m => m Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/DocumentTemplate) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_document_template_asset_delete \
        \    AFTER DELETE \
        \    ON document_template_asset \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_document_template_asset_delete(); \
        \CREATE OR REPLACE TRIGGER trigger_on_after_document_template_locale_delete \
        \    AFTER DELETE \
        \    ON document_template_locale \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION create_persistent_command_from_entity_uuid('document_template_locale', 'deleteFromS3');"
  let action conn = execute_ conn sql
  runDB action
