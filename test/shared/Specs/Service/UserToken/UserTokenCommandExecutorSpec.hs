module Specs.Service.UserToken.UserTokenCommandExecutorSpec where

import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.Cache.ServerCache
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.UserToken.RevokeUserTokensCommand
import Shared.Model.User.User
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.PersistentCommand.WizardPersistentCommandService
import Shared.Util.JSON (encodeJsonToString)
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Specs.Common

userTokenCommandExecutorSpec requestContext =
  describe "UserTokenCommandExecutor" $
    describe "revoke" $ do
      it "deletes every token of the user and keeps the tokens of another user" $
        -- GIVEN: Prepare users and tokens
        do
          runInContextIO U_Migration.runMigration requestContext
          runInContextIO (insertUserToken albertApiKey) requestContext
          runInContextIO (insertUserToken albertMcpToken) requestContext
          -- AND: Prepare command
          command <- insertRevokeCommand requestContext (encodeJsonToString RevokeUserTokensCommand {userUuid = userAlbert.uuid})
          -- WHEN:
          (Right _) <- runInContext (runPersistentCommandById command.uuid) requestContext
          -- THEN:
          resCommand <- findCommand requestContext command.uuid
          resCommand.state `shouldBe` DonePersistentCommandState
          (Right albertTokens) <- runInContext (findUserTokensByUserUuid userAlbert.uuid) requestContext
          albertTokens `shouldBe` []
          (Right nikolaTokens) <- runInContext (findUserTokensByUserUuid userNikola.uuid) requestContext
          nikolaTokens `shouldBe` [nikolaToken]
      it "fails on an invalid body" $
        -- GIVEN: Prepare command
        do
          command <- insertRevokeCommand requestContext "{}"
          -- WHEN:
          (Right _) <- runInContext (runPersistentCommandById command.uuid) requestContext
          -- THEN:
          resCommand <- findCommand requestContext command.uuid
          resCommand.state `shouldBe` ErrorPersistentCommandState
          fmap (L.isPrefixOf "Problem in deserialization of JSON") resCommand.lastErrorMessage `shouldBe` Just True
          (Right albertTokens) <- runInContext (findUserTokensByUserUuid userAlbert.uuid) requestContext
          albertTokens `shouldBe` [albertToken]

insertRevokeCommand :: RequestContext -> String -> IO (PersistentCommand U.UUID)
insertRevokeCommand requestContext body = do
  uuid <- generateUuid
  now <- getCurrentTime
  let command = toPersistentCommand uuid "user_token" "revoke" body 10 defaultTenantUuid Nothing now
  runInContextIO (insertPersistentCommand command) requestContext
  return command

findCommand :: RequestContext -> U.UUID -> IO (PersistentCommand U.UUID)
findCommand requestContext uuid = do
  (Right command) <- runInContext (findPersistentCommandByUuid uuid) requestContext
  return command
