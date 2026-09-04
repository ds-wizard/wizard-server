module WizardLib.Public.Database.Migration.Development.User.UserOpenIdIdentitySchemaMigration where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/UserOpenIdIdentity) drop tables"
  prefix <- tablePrefix
  let sql = f'' "DROP TABLE IF EXISTS ${p}user_openid_identity CASCADE;" [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/UserOpenIdIdentity) create table"
  prefix <- tablePrefix
  let sql =
        f''
          "CREATE TABLE ${p}user_openid_identity \
          \( \
          \    uuid           uuid        NOT NULL, \
          \    external_id    varchar     NOT NULL, \
          \    external_label varchar, \
          \    user_uuid      uuid        NOT NULL, \
          \    provider_uuid  uuid        NOT NULL, \
          \    tenant_uuid    uuid        NOT NULL, \
          \    created_at     timestamptz NOT NULL, \
          \    CONSTRAINT ${p}user_openid_identity_pk PRIMARY KEY (uuid), \
          \    CONSTRAINT ${p}user_openid_identity_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES ${p}user_entity (uuid) ON DELETE CASCADE, \
          \    CONSTRAINT ${p}user_openid_identity_provider_uuid_fk FOREIGN KEY (provider_uuid) REFERENCES ${p}openid_client (uuid) ON DELETE CASCADE, \
          \    CONSTRAINT ${p}user_openid_identity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
          \); \
          \CREATE UNIQUE INDEX ${p}user_openid_identity_uindex ON ${p}user_openid_identity (external_id, provider_uuid, tenant_uuid);"
          [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action
