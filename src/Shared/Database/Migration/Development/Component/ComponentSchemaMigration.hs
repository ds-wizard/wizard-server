module Shared.Database.Migration.Development.Component.ComponentSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Constant.Component
import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Component) drop table"
  let sql = "DROP TABLE IF EXISTS component CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Component) create table"
  let sql =
        "CREATE TABLE component \
        \( \
        \    name       varchar     NOT NULL, \
        \    version    varchar     NOT NULL, \
        \    built_at   timestamptz NOT NULL, \
        \    created_at timestamptz NOT NULL, \
        \    updated_at timestamptz NOT NULL, \
        \    CONSTRAINT component_pk PRIMARY KEY (name) \
        \);"
  let action conn = execute_ conn sql
  runDB action
