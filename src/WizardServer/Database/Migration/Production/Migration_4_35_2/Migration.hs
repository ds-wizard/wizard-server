module WizardServer.Database.Migration.Production.Migration_4_35_2.Migration (
  definition,
  roleUuidMapping,
  remapRoleUuids,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.List (intercalate)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 4035002, mmName = "Valid role uuids", mmDescription = "Replace the seeded role uuids that are not valid RFC 4122 uuids"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  remapRoleUuids roleUuidMapping dbPool
  return Nothing

roleUuidMapping :: [(String, String)]
roleUuidMapping =
  [ ("a0000000-0000-0000-0000-000000000001", "378bdd91-96e4-44ad-9b39-25cd3cdde18d")
  , ("a0000000-0000-0000-0000-000000000002", "65f88e18-8a24-4f70-92e7-8f20f7559600")
  , ("a0000000-0000-0000-0000-000000000003", "4e79c242-4100-4447-becd-45eca47fa60b")
  ]

remapRoleUuids :: [(String, String)] -> Pool Connection -> LoggingT IO ()
remapRoleUuids mapping dbPool = do
  let values = intercalate ", " . fmap (\(old, new) -> "('" ++ old ++ "'::uuid, '" ++ new ++ "'::uuid)") $ mapping
  let sql =
        fromString $
          "ALTER TABLE user_entity DROP CONSTRAINT user_entity_role_uuid_fk; \
          \UPDATE role SET uuid = m.new FROM (VALUES "
            ++ values
            ++ ") AS m(old, new) WHERE role.uuid = m.old; \
               \UPDATE user_entity SET role_uuid = m.new FROM (VALUES "
            ++ values
            ++ ") AS m(old, new) WHERE user_entity.role_uuid = m.old; \
               \UPDATE config_authentication SET default_role_uuid = m.new FROM (VALUES "
            ++ values
            ++ ") AS m(old, new) WHERE config_authentication.default_role_uuid = m.old; \
               \ALTER TABLE user_entity ADD CONSTRAINT user_entity_role_uuid_fk FOREIGN KEY (role_uuid) REFERENCES role (uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()
