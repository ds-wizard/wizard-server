module Specs.Database.DAO.PersistentCommand.PersistentCommandDAOSpec where

import Control.Monad (void)
import qualified Data.List as L
import qualified Data.UUID as U
import Test.Hspec

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandSimple
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.PersistentCommand
import Specs.Common

persistentCommandDAOSpec requestContext =
  describe "PersistentCommand DAO" $
    it "marks the commands it hands over to a Lambda and skips them on the next retry" $ do
      -- GIVEN: Prepare commands
      runInContextIO deletePersistentCommands requestContext
      runInContextIO (traverse insertPersistentCommand [newCommand, errorCommand, invokedCommand, doneCommand, otherComponentCommand]) requestContext
      runInContextIO (updatePersistentCommandLambdaInvokedByUuid invokedCommand.uuid True) requestContext
      -- WHEN: Mark commands
      (Right marked) <- runInContext (markPersistentCommandsForLambdaRetry ["mailer"] :: RequestContextM [PersistentCommandSimple U.UUID]) requestContext
      -- THEN: Check marked commands
      L.sort (fmap (.uuid) marked) `shouldBe` L.sort [newCommand.uuid, errorCommand.uuid]
      (Right lambdaInvoked) <- runInContext (findLambdaInvoked newCommand.uuid) requestContext
      lambdaInvoked `shouldBe` [True]
      (Right command) <- runInContext (findPersistentCommandByUuid newCommand.uuid :: RequestContextM (PersistentCommand U.UUID)) requestContext
      command `shouldBe` newCommand
      -- AND: Check the next retry
      (Right markedAgain) <- runInContext (markPersistentCommandsForLambdaRetry ["mailer"] :: RequestContextM [PersistentCommandSimple U.UUID]) requestContext
      fmap (.uuid) markedAgain `shouldBe` []
      -- CLEAN UP
      void $ runInContextIO deletePersistentCommands requestContext

newCommand :: PersistentCommand U.UUID
newCommand = command1 {uuid = u' "1c8f2e4a-6b3d-4f7e-9a1c-2d5e8f0b3a61", state = NewPersistentCommandState, component = "mailer"}

errorCommand :: PersistentCommand U.UUID
errorCommand = command1 {uuid = u' "2d9a3f5b-7c4e-4a8f-8b2d-3e6f9a1c4b72", state = ErrorPersistentCommandState, component = "mailer"}

invokedCommand :: PersistentCommand U.UUID
invokedCommand = command1 {uuid = u' "3e0b4a6c-8d5f-4b9a-9c3e-4f7a0b2d5c83", state = NewPersistentCommandState, component = "mailer"}

doneCommand :: PersistentCommand U.UUID
doneCommand = command1 {uuid = u' "4f1c5b7d-9e6a-4c0b-8d4f-5a8b1c3e6d94", component = "mailer"}

otherComponentCommand :: PersistentCommand U.UUID
otherComponentCommand = command1 {uuid = u' "5a2d6c8e-0f7b-4d1c-9e5a-6b9c2d4f7ea5", state = NewPersistentCommandState, component = "document_worker"}
