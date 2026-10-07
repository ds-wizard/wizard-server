module Shared.Database.Migration.Development.Registry.RegistrySchemaMigration where

import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger
import Shared.Util.String (f'')

dropTables :: WizardRequestContextC s m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Registry) drop tables"
  let sql =
        "DROP TABLE IF EXISTS registry_knowledge_model_package CASCADE; \
        \DROP TABLE IF EXISTS registry_document_template CASCADE; \
        \DROP TABLE IF EXISTS registry_locale CASCADE; \
        \DROP TABLE IF EXISTS registry_mail_template_locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: WizardRequestContextC s m => m Int64
createTables = do
  createKnowledgeModelPackageTable
  createTemplateTable
  createLocaleTable "registry_locale"
  createLocaleTable "registry_mail_template_locale"

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

createLocaleTable :: WizardRequestContextC s m => String -> m Int64
createLocaleTable table = do
  logInfo _CMP_MIGRATION (f' "(Table/Registry) create table %s" [table])
  let sql =
        fromString $
          f''
            "CREATE TABLE ${table} \
            \( \
            \    code            varchar     NOT NULL, \
            \    name            varchar     NOT NULL, \
            \    remote_version  varchar     NOT NULL, \
            \    created_at      timestamptz NOT NULL, \
            \    CONSTRAINT ${table}_pk PRIMARY KEY (code) \
            \);"
            [("table", table)]
  let action conn = execute_ conn sql
  runDB action
