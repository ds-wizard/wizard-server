module Specs.Database.Migration.Production.Migration_5_0_0.MigrationSpec where

import Control.Monad (void)
import Control.Monad.Logger (runLoggingT)
import qualified Data.List as L
import qualified Data.UUID as U
import Test.Hspec

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.Migration.Development.PersistentCommand.Data.WizardPersistentCommands
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Uuid
import WizardServer.Database.Migration.Production.Migration_5_0_0.Migration
import WizardServer.Model.Context.RequestContext

import Specs.Common

migration_5_0_0Spec requestContext =
  describe "Migration 5.0.0" $
    it "renames the stored persistent command components" $ do
      -- GIVEN: Prepare commands with the old components
      runInContextIO deletePersistentCommands requestContext
      runInContextIO (traverse insertPersistentCommand oldCommands) requestContext
      -- WHEN: Run migration step
      runLoggingT (renamePersistentCommandComponents requestContext.dbPool) (\_ _ _ _ -> return ())
      -- THEN: Check components
      (Right commands) <- runInContext (findPersistentCommands :: RequestContextM [PersistentCommand U.UUID]) requestContext
      L.sort (fmap (.component) commands) `shouldBe` ["aws_event_bridge", "component1", "document_worker", "locale", "mailer"]
      -- CLEAN UP
      void $ runInContextIO deletePersistentCommands requestContext

oldCommands :: [PersistentCommand U.UUID]
oldCommands =
  [ command1
  , command1 {uuid = u' "5e1c2a7b-3d4f-4a6b-8c9d-0e1f2a3b4c5d", component = "doc_worker"}
  , command1 {uuid = u' "6f2d3b8c-4e5a-4b7c-9d0e-1f2a3b4c5d6e", component = "mailer"}
  , command1 {uuid = u' "7a3e4c9d-5f6b-4c8d-0e1f-2a3b4c5d6e7f", component = "locale"}
  , command1 {uuid = u' "8b4f5d0e-6a7c-4d9e-1f2a-3b4c5d6e7f80", component = "awsEventBridge"}
  ]
