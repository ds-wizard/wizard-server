module Shared.Database.Migration.Development.Registry.RegistrySchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Registry) drop tables"
  let sql =
        "DROP TABLE IF EXISTS registry_knowledge_model_package CASCADE; \
        \DROP TABLE IF EXISTS registry_document_template CASCADE; \
        \DROP TABLE IF EXISTS registry_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createKnowledgeModelPackageTable
  createTemplateTable
  createLocaleTable

createKnowledgeModelPackageTable :: WizardRequestContextC s m => m Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE registry_knowledge_model_package \
        \( \
        \    id              varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_knowledge_model_package_pk PRIMARY KEY (id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateTable :: WizardRequestContextC s m => m Int64
createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE registry_document_template \
        \( \
        \    id              varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_document_template_pk PRIMARY KEY (id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createLocaleTable :: WizardRequestContextC s m => m Int64
createLocaleTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryLocale) create table"
  let sql =
        "CREATE TABLE registry_locale \
        \( \
        \    id              varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_locale_pk PRIMARY KEY (id) \
        \);"
  let action conn = execute_ conn sql
  runDB action
