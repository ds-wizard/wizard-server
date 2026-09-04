module Wizard.Specs.Service.PersistentCommand.PersistentCommandServiceSpec where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U
import Test.Hspec

import Shared.Common.Constant.Tenant
import Shared.Common.Constant.User
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.Roles
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.PersistentCommand.PersistentCommandService
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand
import WizardLib.Public.Model.User.Role

import Wizard.Specs.Common

persistentCommandServiceSpec appContext =
  describe "Persistent Command Service" $
    it "runs a command inserted by admin" $
      -- GIVEN:
      do
        runInContextIO U_Migration.runMigration appContext
        runInContextIO (insertPersistentCommand createUserCommand) appContext
        -- WHEN:
        (Right ()) <- runInContext runPersistentCommands' appContext
        -- THEN:
        (Right user) <- runInContext (findUserByUuid userUuid) appContext
        user.email `shouldBe` "marie.curie@example.com"
        (Right command) <- runInContext (findPersistentCommandByUuid createUserCommand.uuid :: AppContextM (PersistentCommand U.UUID)) appContext
        command.state `shouldBe` DonePersistentCommandState

userUuid = u' "e1c58e52-0824-4526-8ebe-ec38eec67030"

createUserCommand :: PersistentCommand U.UUID
createUserCommand =
  toPersistentCommand
    (u' "0b6bbcbc-7b8b-4b64-a0a4-1a2e2fd1e2c6")
    "user"
    "createUser"
    ( BSL.unpack . encode $
        CreateOrUpdateUserCommand
          { uuid = userUuid
          , firstName = "Marie"
          , lastName = "Curie"
          , email = "marie.curie@example.com"
          , affiliation = Nothing
          , roleUuid = adminRole.uuid
          , active = True
          , imageUrl = Nothing
          , tenantUuid = defaultTenantUuid
          }
    )
    10
    True
    Nothing
    defaultTenantUuid
    (Just systemUserUuid)
    (dt' 2018 1 25)
