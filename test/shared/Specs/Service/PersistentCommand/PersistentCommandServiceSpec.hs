module Specs.Service.PersistentCommand.PersistentCommandServiceSpec where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Pool (withResource)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Constant.User
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Model.Locale.Locale
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.Project.RefreshProjectCacheCommand
import Shared.Model.PersistentCommand.Trigger.TriggerEntityUuidCommand
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Util.Date
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Common

persistentCommandServiceSpec requestContext =
  describe "Persistent Command Service" $ do
    it "runs a command inserted by a trigger" $
      -- GIVEN:
      do
        runInContextIO (insertPersistentCommand deleteLocaleCommand) requestContext
        -- WHEN:
        (Right ()) <- runInContext runPersistentCommands' requestContext
        -- THEN:
        (Right command) <- runInContext (findPersistentCommandByUuid deleteLocaleCommand.uuid :: RequestContextM (PersistentCommand U.UUID)) requestContext
        command.state `shouldBe` DonePersistentCommandState

    it "does not run a command that another runner is already running" $
      -- GIVEN:
      do
        runInContextIO (insertPersistentCommand deleteLocaleCommand) requestContext
        withResource requestContext.dbPool $ \connection ->
          withTransaction connection $ do
            _ <- query connection "SELECT uuid FROM persistent_command WHERE uuid = ? FOR UPDATE" (Only deleteLocaleCommand.uuid) :: IO [Only U.UUID]
            -- WHEN:
            (Right ()) <- runInContext runPersistentCommands' requestContext
            return ()
        -- THEN:
        (Right command) <- runInContext (findPersistentCommandByUuid deleteLocaleCommand.uuid :: RequestContextM (PersistentCommand U.UUID)) requestContext
        command.state `shouldBe` NewPersistentCommandState

    it "refreshes the project cache and hands over to analytics" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO (insertPersistentCommand refreshProjectCacheCommand) requestContext
        -- WHEN:
        (Right ()) <- runInContext runPersistentCommands' requestContext
        -- THEN:
        (Right command) <- runInContext (findPersistentCommandByUuid refreshProjectCacheCommand.uuid :: RequestContextM (PersistentCommand U.UUID)) requestContext
        command.state `shouldBe` DonePersistentCommandState
        (Right projects) <- runInContext findProjects requestContext
        (Right caches) <- runInContext findProjectCaches requestContext
        length caches `shouldBe` length projects
        (Right commands) <- runInContext (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
        let [analyticsCommand] = filter (\c -> c.component == "analytics") commands
        analyticsCommand.function `shouldBe` "synchronize"
        analyticsCommand.body `shouldBe` refreshProjectCacheCommand.body
        analyticsCommand.state `shouldBe` NewPersistentCommandState

refreshProjectCacheCommand :: PersistentCommand U.UUID
refreshProjectCacheCommand =
  toPersistentCommand
    (u' "3d9d1b8c-6f1e-4a2b-9c0d-5e7f8a9b0c1d")
    "project_cache"
    "refresh"
    (BSL.unpack . encode $ RefreshProjectCacheCommand {tenantUuid = defaultTenantUuid})
    10
    defaultTenantUuid
    (Just systemUserUuid)
    (dt' 2018 1 25)

deleteLocaleCommand :: PersistentCommand U.UUID
deleteLocaleCommand =
  toPersistentCommand
    (u' "0b6bbcbc-7b8b-4b64-a0a4-1a2e2fd1e2c6")
    "locale"
    "deleteFromS3"
    (BSL.unpack . encode $ TriggerEntityUuidCommand {uuid = localeNl.uuid})
    10
    defaultTenantUuid
    (Just systemUserUuid)
    (dt' 2018 1 25)
