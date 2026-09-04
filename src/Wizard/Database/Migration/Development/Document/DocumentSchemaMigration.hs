module Wizard.Database.Migration.Development.Document.DocumentSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Document) drop tables"
  let sql = "DROP TABLE IF EXISTS w_document CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropTriggers :: AppContextM Int64
dropTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Document) drop tables"
  let sql = "DROP TRIGGER IF EXISTS trigger_on_after_document_delete ON w_document;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createDocumentTable
  createPersistentCommandFromDocumentDeleteFunction

createDocumentTable = do
  logInfo _CMP_MIGRATION "(Table/Document) create table"
  let sql =
        "CREATE TABLE w_document \
        \( \
        \    uuid                   uuid        NOT NULL, \
        \    name                   varchar     NOT NULL, \
        \    state                  varchar     NOT NULL, \
        \    durability             varchar     NOT NULL, \
        \    project_uuid           uuid, \
        \    project_event_uuid     uuid, \
        \    project_replies_hash   bigint      NOT NULL, \
        \    document_template_uuid uuid        NOT NULL, \
        \    format_uuid            uuid        NOT NULL, \
        \    created_by             uuid, \
        \    retrieved_at           timestamptz, \
        \    finished_at            timestamptz, \
        \    created_at             timestamptz NOT NULL, \
        \    file_name              varchar, \
        \    content_type           varchar, \
        \    worker_log             varchar, \
        \    tenant_uuid            uuid        NOT NULL, \
        \    file_size              bigint, \
        \    CONSTRAINT w_document_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT w_document_project_uuid_fk FOREIGN KEY (project_uuid) REFERENCES w_project (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_document_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES w_document_template (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_document_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES w_document_template_format (document_template_uuid, uuid) ON DELETE CASCADE, \
        \    CONSTRAINT w_document_created_by_fk FOREIGN KEY (created_by) REFERENCES w_user_entity (uuid) ON DELETE SET NULL, \
        \    CONSTRAINT w_document_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createPersistentCommandFromDocumentDeleteFunction = do
  let sql =
        "CREATE OR REPLACE FUNCTION w_create_persistent_command_from_document_delete() \
        \    RETURNS TRIGGER AS \
        \$$ \
        \BEGIN \
        \    PERFORM w_create_persistent_command( \
        \            'document', \
        \            'deleteFromS3', \
        \            jsonb_build_object('uuid', OLD.uuid), \
        \            OLD.tenant_uuid); \
        \    RETURN OLD; \
        \END; \
        \$$ LANGUAGE plpgsql;"
  let action conn = execute_ conn sql
  runDB action

createTriggers :: AppContextM Int64
createTriggers = do
  logInfo _CMP_MIGRATION "(Trigger/Document) create triggers"
  let sql =
        "CREATE OR REPLACE TRIGGER trigger_on_after_document_delete \
        \    AFTER DELETE \
        \    ON w_document \
        \    FOR EACH ROW \
        \EXECUTE FUNCTION w_create_persistent_command_from_document_delete();"
  let action conn = execute_ conn sql
  runDB action
