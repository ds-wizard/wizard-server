module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelCache) drop tables"
  let sql = "DROP TABLE IF EXISTS knowledge_model_cache CASCADE; "
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelCache) create tables"
  let sql =
        "CREATE TABLE knowledge_model_cache \
        \( \
        \    package_uuid               uuid        NOT NULL, \
        \    tag_uuids                  text[]      NOT NULL, \
        \    knowledge_model            jsonb       NOT NULL, \
        \    tenant_uuid                uuid        NOT NULL, \
        \    created_at                 timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_cache_pk PRIMARY KEY (package_uuid, tag_uuids), \
        \    CONSTRAINT knowledge_model_cache_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT knowledge_model_cache_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action
