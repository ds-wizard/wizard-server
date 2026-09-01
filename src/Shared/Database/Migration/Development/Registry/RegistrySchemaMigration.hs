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
        "DROP TABLE IF EXISTS registry_organization CASCADE; \
        \DROP TABLE IF EXISTS registry_knowledge_model_package CASCADE; \
        \DROP TABLE IF EXISTS registry_document_template CASCADE; \
        \DROP TABLE IF EXISTS registry_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createOrganizationTable
  createKnowledgeModelPackageTable
  createTemplateTable
  createLocaleTable

createOrganizationTable :: WizardRequestContextC s m => m Int64
createOrganizationTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryOrganization) create table"
  let sql =
        "CREATE TABLE registry_organization \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    name            varchar     NOT NULL, \
        \    logo            varchar, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_organization_pk PRIMARY KEY (organization_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageTable :: WizardRequestContextC s m => m Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE registry_knowledge_model_package \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    km_id           varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_knowledge_model_package_pk PRIMARY KEY (organization_id, km_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateTable :: WizardRequestContextC s m => m Int64
createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE registry_document_template \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    template_id     varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_document_template_pk PRIMARY KEY (organization_id, template_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createLocaleTable :: WizardRequestContextC s m => m Int64
createLocaleTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryLocale) create table"
  let sql =
        "CREATE TABLE registry_locale \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    locale_id       varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT registry_locale_pk PRIMARY KEY (organization_id, locale_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action
