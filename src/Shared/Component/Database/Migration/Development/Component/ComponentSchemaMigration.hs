module Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Constant.Component
import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Component) drop table"
  prefix <- tablePrefix
  let sql = f'' "DROP TABLE IF EXISTS ${p}component CASCADE;" [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Component) create table"
  prefix <- tablePrefix
  let sql =
        f''
          "CREATE TABLE ${p}component \
          \( \
          \    name       varchar     NOT NULL, \
          \    version    varchar     NOT NULL, \
          \    built_at   timestamptz NOT NULL, \
          \    created_at timestamptz NOT NULL, \
          \    updated_at timestamptz NOT NULL, \
          \    CONSTRAINT ${p}component_pk PRIMARY KEY (name) \
          \);"
          [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action
