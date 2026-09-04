module Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration where

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
  logInfo _CMP_MIGRATION "(Table/Audit) drop tables"
  prefix <- tablePrefix
  let sql = f'' "DROP TABLE IF EXISTS ${p}audit CASCADE;" [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  prefix <- tablePrefix
  let sql =
        f''
          "CREATE TABLE ${p}audit \
          \( \
          \    uuid        uuid        NOT NULL, \
          \    component   varchar     NOT NULL, \
          \    action      varchar     NOT NULL, \
          \    entity      varchar     NOT NULL, \
          \    body        jsonb       NOT NULL, \
          \    created_by  uuid, \
          \    tenant_uuid uuid        NOT NULL, \
          \    created_at  timestamptz NOT NULL, \
          \    CONSTRAINT ${p}audit_pk PRIMARY KEY (uuid), \
          \    CONSTRAINT ${p}audit_created_by_fk FOREIGN KEY (created_by) REFERENCES ${p}user_entity (uuid) ON DELETE CASCADE, \
          \    CONSTRAINT ${p}audit_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
          \);"
          [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action
