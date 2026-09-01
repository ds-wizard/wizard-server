module Specs.Service.KnowledgeModel.Editor.KnowledgeModelEditorServiceSpec where

import Control.Monad.Reader
import Data.Either
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

import Specs.Common

knowledgeModelEditorServiceSpec requestContext =
  describe "Knowledge Model Editor Service Integration" $ do
    describe "getEditorState" $ do
      it "DefaultKnowledgeModelEditorState - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          runInContext (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = DefaultKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "EditedKnowledgeModelEditorState - edit events" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = EditedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "EditedKnowledgeModelEditorState - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = EditedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "OutdatedKnowledgeModelEditorState - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = OutdatedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "MigratingKnowledgeModelEditorState - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          runInContext (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditor.uuid) requestContext
          let migratorCreateDto =
                KnowledgeModelMigrationCreateDTO {targetPackageUuid = netherlandsKmPackageV2.uuid}
          runInContext (createMigration amsterdamKnowledgeModelEditor.uuid migratorCreateDto) requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = amsterdamKnowledgeModelEditorEvents
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = MigratingKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
      it "MigratedKnowledgeModelEditorState - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
        do
          runInContext KnowledgeModelPackage.runMigration requestContext
          runInContext KnowledgeModelEditor.runMigration requestContext
          let migratorCreateDto =
                KnowledgeModelMigrationCreateDTO {targetPackageUuid = netherlandsKmPackageV2.uuid}
          runInContext (createMigration amsterdamKnowledgeModelEditor.uuid migratorCreateDto) requestContext
          let reqDto =
                KnowledgeModelMigrationResolutionDTO
                  { originalEventUuid = a_km1_ch4.uuid
                  , action = RejectKnowledgeModelMigrationAction
                  }
          runInContext (solveConflictAndMigrate amsterdamKnowledgeModelEditor.uuid reqDto) requestContext
          -- AND: Prepare KM editor
          let editor = amsterdamKnowledgeModelEditor
          let kmEditorEvents = []
          let forkOfPackageId = Just . createCoordinate $ netherlandsKmPackage
          -- AND: Prepare expectations
          let expState = MigratedKnowledgeModelEditorState
          -- WHEN:
          eitherResState <- runInContext (getEditorState editor (length kmEditorEvents) forkOfPackageId) requestContext
          -- THEN:
          liftIO $ isRight eitherResState `shouldBe` True
          let (Right resState) = eitherResState
          resState `shouldBe` expState
