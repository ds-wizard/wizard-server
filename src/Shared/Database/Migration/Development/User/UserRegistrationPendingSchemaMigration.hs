module Shared.Database.Migration.Development.User.UserRegistrationPendingSchemaMigration where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/UserRegistrationPending) drop tables"
  let sql = "DROP TABLE IF EXISTS user_registration_pending CASCADE;"
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: RequestContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/UserRegistrationPending) create table"
  let sql =
        "CREATE TABLE user_registration_pending \
        \( \
        \    uuid           uuid        NOT NULL, \
        \    hash           varchar     NOT NULL, \
        \    service_type   varchar     NOT NULL, \
        \    provider_uuid  uuid        NOT NULL, \
        \    external_id    varchar     NOT NULL, \
        \    external_label varchar, \
        \    email          varchar, \
        \    first_name     varchar, \
        \    last_name      varchar, \
        \    image_url      varchar, \
        \    affiliation    varchar, \
        \    tenant_uuid    uuid        NOT NULL, \
        \    created_at     timestamptz NOT NULL, \
        \    CONSTRAINT user_registration_pending_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_registration_pending_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES tenant (uuid) ON DELETE CASCADE \
        \); \
        \CREATE UNIQUE INDEX user_registration_pending_hash_uindex ON user_registration_pending (hash, tenant_uuid);"
  let action conn = execute_ conn (fromString sql)
  runDB action
