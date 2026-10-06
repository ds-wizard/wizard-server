module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Package) drop tables"
  let sql =
        "DROP TABLE IF EXISTS knowledge_model_package_event CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_package CASCADE;"
  let action conn = execute_ conn sql
  runDB action

dropFunctions :: WizardRequestContextC s m => m Int64
dropFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) drop functions"
  let sql =
        "DROP FUNCTION IF EXISTS get_newest_knowledge_model_package; \
        \DROP FUNCTION IF EXISTS get_newest_knowledge_model_package_coordinate; \
        \DROP FUNCTION IF EXISTS get_organization_id; \
        \DROP FUNCTION IF EXISTS get_km_id;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createKnowledgeModelPackageTable
  createKnowledgeModelPackageEventTable

createKnowledgeModelPackageTable :: WizardRequestContextC s m => m Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/Package) create table"
  let sql =
        "CREATE TABLE knowledge_model_package \
        \( \
        \    uuid                        uuid        NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    id                          varchar     NOT NULL, \
        \    version                     varchar     NOT NULL, \
        \    metamodel_version           integer     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    readme                      varchar     NOT NULL, \
        \    license                     varchar     NOT NULL, \
        \    previous_package_uuid       uuid, \
        \    fork_of_package_id          varchar, \
        \    merge_checkpoint_package_id varchar, \
        \    created_at                  timestamptz NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    phase                       varchar     NOT NULL, \
        \    non_editable                bool        NOT NULL, \
        \    public                      bool        NOT NULL, \
        \    language                    varchar     NOT NULL DEFAULT 'en', \
        \    workspace_uuid              uuid, \
        \    fork_of_package_version     varchar, \
        \    merge_checkpoint_package_version varchar, \
        \    CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT knowledge_model_package_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_workspace_uuid_fk FOREIGN KEY (workspace_uuid) REFERENCES workspace (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_coordinate_unique UNIQUE NULLS NOT DISTINCT (id, version, tenant_uuid, workspace_uuid) \
        \); \
        \ \
        \CREATE INDEX knowledge_model_package_id_index ON knowledge_model_package (id, tenant_uuid); \
        \ \
        \CREATE INDEX knowledge_model_package_previous_package_uuid_index ON knowledge_model_package (previous_package_uuid); \
        \CREATE INDEX knowledge_model_package_workspace_uuid_index ON knowledge_model_package (workspace_uuid);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageEventTable :: WizardRequestContextC s m => m Int64
createKnowledgeModelPackageEventTable = do
  logInfo _CMP_MIGRATION "(Table/PackageEvent) create table"
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_package_event \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    parent_uuid  uuid        NOT NULL, \
        \    entity_uuid  uuid        NOT NULL, \
        \    content      jsonb       NOT NULL, \
        \    package_uuid uuid     NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_uuid), \
        \    CONSTRAINT knowledge_model_package_event_package_id_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_package_event_tenant_uuid FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

createFunctions :: WizardRequestContextC s m => m Int64
createFunctions = do
  logInfo _CMP_MIGRATION "(Function/Package) create functions"
  createGetNewestPackageFn

createGetNewestPackageFn :: WizardRequestContextC s m => m Int64
createGetNewestPackageFn = do
  let sql =
        "CREATE OR REPLACE FUNCTION get_newest_knowledge_model_package(req_id varchar, req_tenant_uuid uuid, req_phase varchar[], req_workspace_uuid uuid) \
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
        \ \
        \    RETURN p_uuid; \
        \END; \
        \$$;"
  let action conn = execute_ conn sql
  runDB action
