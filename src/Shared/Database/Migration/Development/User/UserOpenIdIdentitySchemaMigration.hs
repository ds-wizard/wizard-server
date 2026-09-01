module Shared.Database.Migration.Development.User.UserOpenIdIdentitySchemaMigration where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/UserOpenIdIdentity) drop tables"
  let sql = "DROP TABLE IF EXISTS user_openid_identity CASCADE;"
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/UserOpenIdIdentity) create table"
  let sql =
        "CREATE TABLE user_openid_identity \
        \( \
        \    uuid           uuid        NOT NULL, \
        \    external_id    varchar     NOT NULL, \
        \    external_label varchar, \
        \    user_uuid      uuid        NOT NULL, \
        \    provider_uuid  uuid        NOT NULL, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    created_at     timestamptz NOT NULL, \
        \    CONSTRAINT user_openid_identity_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_openid_identity_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_openid_identity_provider_uuid_fk FOREIGN KEY (provider_uuid) REFERENCES openid_client (uuid) ON DELETE CASCADE, \
        \    CONSTRAINT user_openid_identity_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE UNIQUE INDEX user_openid_identity_uindex ON user_openid_identity (external_id, provider_uuid, tenant_uuid); \
        \CREATE INDEX user_openid_identity_user_uuid_idx ON user_openid_identity (user_uuid, tenant_uuid);"
  let action conn = execute_ conn (fromString sql)
  runDB action
