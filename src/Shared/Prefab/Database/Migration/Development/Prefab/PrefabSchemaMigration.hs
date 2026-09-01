module Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration where

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
  logInfo _CMP_MIGRATION "(Table/Prefab) drop table"
  prefix <- tablePrefix
  let sql = f'' "DROP TABLE IF EXISTS ${p}prefab CASCADE;" [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Prefab) create table"
  prefix <- tablePrefix
  let sql =
        f''
          "CREATE TABLE ${p}prefab \
          \( \
          \    uuid        uuid        NOT NULL, \
          \    type        varchar     NOT NULL, \
          \    name        varchar     NOT NULL, \
          \    content     jsonb       NOT NULL, \
          \    tenant_uuid uuid        NOT NULL, \
          \    created_at  timestamptz NOT NULL, \
          \    updated_at  timestamptz NOT NULL, \
          \    CONSTRAINT ${p}prefab_pk PRIMARY KEY (uuid, tenant_uuid), \
          \    CONSTRAINT ${p}prefab_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES ${p}tenant (uuid) ON DELETE CASCADE \
          \);"
          [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action
