module Specs.Api.Handler.KnowledgeModelEditor.Migration.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration as KM_MIG
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

import Specs.Common

-- --------------------------------
-- MIGRATION
-- --------------------------------
runMigrationWithEmptyDB requestContext = do
  runInContextIO KnowledgeModelEditor.runMigration requestContext
  runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
  runInContextIO KM_MIG.runMigration requestContext

runMigrationWithFullDB requestContext = do
  runMigrationWithEmptyDB requestContext
  runInContextIO (createMigration amsterdamKnowledgeModelEditorList.uuid knowledgeModelMigrationCreateDTO) requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertStateOfMigrationInDB requestContext kmMigration expState = do
  eKmMigration <- runInContextIO (findKnowledgeModelMigrationByEditorUuid kmMigration.editorUuid) requestContext
  liftIO $ isRight eKmMigration `shouldBe` True
  let (Right kmMigrationFromDB) = eKmMigration
  liftIO $ kmMigrationFromDB.state `shouldBe` expState
