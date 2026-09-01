module Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

dropTables :: AppContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Registry) drop tables"
  let sql =
        "DROP TABLE IF EXISTS w_registry_organization CASCADE; \
        \DROP TABLE IF EXISTS w_registry_knowledge_model_package CASCADE; \
        \DROP TABLE IF EXISTS w_registry_document_template CASCADE; \
        \DROP TABLE IF EXISTS w_registry_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: AppContextM Int64
createTables = do
  createOrganizationTable
  createKnowledgeModelPackageTable
  createTemplateTable
  createLocaleTable

createOrganizationTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryOrganization) create table"
  let sql =
        "CREATE TABLE w_registry_organization \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    name            varchar     NOT NULL, \
        \    logo            varchar, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT w_registry_organization_pk PRIMARY KEY (organization_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE w_registry_knowledge_model_package \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    km_id           varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT w_registry_knowledge_model_package_pk PRIMARY KEY (organization_id, km_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createTemplateTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryPackage) create table"
  let sql =
        "CREATE TABLE w_registry_document_template \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    template_id     varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT w_registry_document_template_pk PRIMARY KEY (organization_id, template_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action

createLocaleTable = do
  logInfo _CMP_MIGRATION "(Table/RegistryLocale) create table"
  let sql =
        "CREATE TABLE w_registry_locale \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    locale_id       varchar     NOT NULL, \
        \    remote_version  varchar     NOT NULL, \
        \    created_at      timestamptz NOT NULL, \
        \    CONSTRAINT w_registry_locale_pk PRIMARY KEY (organization_id, locale_id) \
        \);"
  let action conn = execute_ conn sql
  runDB action
