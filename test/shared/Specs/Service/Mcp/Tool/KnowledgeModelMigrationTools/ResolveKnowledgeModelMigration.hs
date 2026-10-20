module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.ResolveKnowledgeModelMigration (
  resolve_knowledge_model_migration,
) where

import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (resolve_knowledge_model_migration)
-- ------------------------------------------------------------------------
resolve_knowledge_model_migration :: RequestContext -> SpecWith ((), Application)
resolve_knowledge_model_migration requestContext =
  describe "POST /api/mcp (resolve_knowledge_model_migration)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid, "action" .= ("reject" :: String), "event_uuid" .= a_km1_ch4.uuid]

reqBody = toolCallBody "resolve_knowledge_model_migration" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "state" structuredContent `shouldBe` Just (String "Completed")
      liftIO $ (valueField "processedEvents" structuredContent, valueField "conflict" structuredContent) `shouldBe` (Just (toJSON (1 :: Int)), Just Null)
      assertToolOutputSchema reqHeaders "resolve_knowledge_model_migration" structuredContent
      -- AND: Compare state in DB with expectation
      assertStateOfMigrationInDB requestContext knowledgeModelMigrationDTO CompletedKnowledgeModelMigrationState
  it "HTTP 200 OK if all conflicts are applied (completed, released by publish_knowledge_model_editor)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "resolve_knowledge_model_migration" (object (("action" .= ("apply_all" :: String)) : ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "state" =<< structuredContentOf response) `shouldBe` Just (String "Completed")
      Just token <- (stringValue <=< valueField "confirmation_token" <=< structuredContentOf) <$> request reqMethod reqUrl reqHeaders (toolCallBody "publish_knowledge_model_editor" (object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]))
      publishResponse <- request reqMethod reqUrl reqHeaders (toolCallBody "publish_knowledge_model_editor" (object (("confirmation_token" .= token) : ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid])))
      let Just (String packageUuid) = valueField "uuid" =<< structuredContentOf publishResponse
      -- AND: Compare state in DB with expectation
      assertForkOfPackageInDB requestContext (read (T.unpack packageUuid)) (createCoordinate netherlandsKmPackageV2)
      assertAbsenceOfKnowledgeModelMigrationInDB requestContext amsterdamKnowledgeModelEditorList.uuid
  it "HTTP 200 OK if the event is not the current conflict (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "resolve_knowledge_model_migration" (object (["action" .= ("apply" :: String), "event_uuid" .= a_km1_ch1.uuid] ++ ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH)
      -- AND: Run migrations
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertRunningKnowledgeModelMigrationInDB requestContext amsterdamKnowledgeModelEditorList.uuid netherlandsKmPackageV2.uuid
  it "HTTP 200 OK if a conflict is applied without event_uuid (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "resolve_knowledge_model_migration" (object (("action" .= ("apply" :: String)) : ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
