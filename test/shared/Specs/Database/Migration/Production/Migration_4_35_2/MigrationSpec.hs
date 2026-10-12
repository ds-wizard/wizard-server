module Specs.Database.Migration.Production.Migration_4_35_2.MigrationSpec where

import Control.Monad.Logger (runLoggingT)
import Data.Pool (withResource)
import Data.Tuple (swap)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Test.Hspec

import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.User.Role
import WizardServer.Database.Migration.Production.Migration_4_35_2.Migration
import WizardServer.Model.Context.RequestContext

import Specs.Common

migration_4_35_2Spec requestContext =
  describe "Migration 4.35.2" $
    it "replaces the invalid seeded role uuids" $ do
      -- GIVEN: Prepare DB with the uuids of the init migration
      runInContextIO U_Migration.runMigration requestContext
      runLoggingT (remapRoleUuids (fmap swap roleUuidMapping) requestContext.dbPool) (\_ _ _ _ -> return ())
      -- WHEN: Run migration
      _ <- runLoggingT (snd definition requestContext.dbPool) (\_ _ _ _ -> return ())
      -- THEN: Check roles, users and the default role
      roleUuids <- findUuids requestContext "SELECT uuid FROM role WHERE uuid IN (?, ?, ?) ORDER BY name"
      roleUuids `shouldBe` [adminRole.uuid, dataStewardRole.uuid, researcherRole.uuid]
      userRoleUuids <- findUuids requestContext "SELECT DISTINCT role_uuid FROM user_entity WHERE role_uuid IN (?, ?, ?) ORDER BY 1"
      length userRoleUuids `shouldBe` 3
      defaultRoleUuids <- findUuids requestContext "SELECT default_role_uuid FROM config_authentication WHERE default_role_uuid IN (?, ?, ?)"
      defaultRoleUuids `shouldBe` [researcherRole.uuid]

findUuids :: RequestContext -> Query -> IO [U.UUID]
findUuids requestContext sql =
  withResource requestContext.dbPool $ \conn ->
    fmap fromOnly <$> query conn sql (adminRole.uuid, dataStewardRole.uuid, researcherRole.uuid)
