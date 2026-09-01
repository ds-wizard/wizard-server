module WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientSchemaMigration where

import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')

dropTables :: AppContextC s sc m => m Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/OpenIdClient) drop tables"
  prefix <- tablePrefix
  let sql = f'' "DROP TABLE IF EXISTS ${p}openid_client CASCADE;" [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action

createTables :: AppContextC s sc m => m Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/OpenIdClient) create table"
  prefix <- tablePrefix
  let sql =
        f''
          "CREATE TABLE ${p}openid_client \
          \( \
          \    uuid                 uuid        NOT NULL, \
          \    name                 varchar     NOT NULL, \
          \    url                  varchar     NOT NULL, \
          \    client_id            varchar     NOT NULL, \
          \    client_secret        varchar     NOT NULL, \
          \    parameters           jsonb       NOT NULL, \
          \    style                jsonb       NOT NULL, \
          \    tenant_uuid          uuid        NOT NULL, \
          \    created_at           timestamptz NOT NULL, \
          \    updated_at           timestamptz NOT NULL, \
          \    registration_enabled bool        NOT NULL, \
          \    scope_profile        bool        NOT NULL, \
          \    scope_email          bool        NOT NULL, \
          \    CONSTRAINT ${p}openid_client_pk PRIMARY KEY (uuid), \
          \    CONSTRAINT ${p}openid_client_tenant_uuid_fk FOREIGN KEY (tenant_uuid) REFERENCES ${p}tenant (uuid) ON DELETE CASCADE \
          \);"
          [("p", prefix)]
  let action conn = execute_ conn (fromString sql)
  runDB action
