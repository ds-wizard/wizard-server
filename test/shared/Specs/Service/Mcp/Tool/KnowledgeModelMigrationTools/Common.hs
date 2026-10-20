module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common where

import Data.Aeson (Value (..), toJSON)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertConflict :: Value -> (Int, Int) -> KnowledgeModelEvent -> WaiSession () ()
assertConflict structuredContent (expProcessed, expTotal) expEvent = do
  liftIO $ (valueField "processedEvents" structuredContent, valueField "totalEvents" structuredContent) `shouldBe` (Just (toJSON expProcessed), Just (toJSON expTotal))
  liftIO $ (valueField "eventUuid" =<< valueField "conflict" structuredContent) `shouldBe` Just (toJSON expEvent.uuid)
  liftIO $ (valueField "entityUuid" =<< valueField "thirdPartyContent" =<< valueField "event" =<< valueField "conflict" structuredContent) `shouldBe` Just (toJSON expEvent.entityUuid)

assertRunningKnowledgeModelMigrationInDB :: RequestContext -> U.UUID -> U.UUID -> WaiSession () ()
assertRunningKnowledgeModelMigrationInDB requestContext editorUuid expTargetPackageUuid = do
  migration <- getOneFromDB (findKnowledgeModelMigrationByEditorUuid editorUuid) requestContext
  liftIO $ migration.targetPackageUuid `shouldBe` expTargetPackageUuid
  liftIO $ migration.state `shouldNotBe` CompletedKnowledgeModelMigrationState

assertAbsenceOfKnowledgeModelMigrationInDB :: RequestContext -> U.UUID -> WaiSession () ()
assertAbsenceOfKnowledgeModelMigrationInDB requestContext editorUuid = do
  eMigration <- runInContextIO (findKnowledgeModelMigrationByEditorUuid' editorUuid) requestContext
  liftIO $ fmap isNothing eMigration `shouldBe` Right True

assertForkOfPackageInDB :: RequestContext -> U.UUID -> Coordinate -> WaiSession () ()
assertForkOfPackageInDB requestContext packageUuid expForkOf = do
  package <- getOneFromDB (findPackageByUuid packageUuid) requestContext
  liftIO $ package.forkOfPackageId `shouldBe` Just expForkOf

assertProjectPackageInDB :: RequestContext -> Project -> U.UUID -> WaiSession () ()
assertProjectPackageInDB requestContext project expPackageUuid = do
  projectFromDb <- getOneFromDB (findProjectByUuid project.uuid) requestContext
  liftIO $ (projectFromDb.knowledgeModelPackageUuid, projectFromDb.selectedQuestionTagUuids) `shouldBe` (expPackageUuid, project.selectedQuestionTagUuids)

-- --------------------------------
-- HELPERS
-- --------------------------------
thirdPartyArray :: String -> Value -> Maybe [Value]
thirdPartyArray name content =
  case valueField "thirdPartyContent" =<< valueField name =<< valueField "conflict" content of
    Just (Array items) -> Just (V.toList items)
    _ -> Nothing

stringValue :: Value -> Maybe String
stringValue (String text) = Just (T.unpack text)
stringValue _ = Nothing
