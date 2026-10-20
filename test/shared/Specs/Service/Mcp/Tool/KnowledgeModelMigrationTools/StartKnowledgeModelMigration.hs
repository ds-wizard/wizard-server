module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.StartKnowledgeModelMigration (
  start_knowledge_model_migration,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (start_knowledge_model_migration)
-- ------------------------------------------------------------------------
start_knowledge_model_migration :: RequestContext -> SpecWith ((), Application)
start_knowledge_model_migration requestContext =
  describe "POST /api/mcp (start_knowledge_model_migration)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid, "target_package_uuid" .= netherlandsKmPackageV2.uuid]

reqBody = toolCallBody "start_knowledge_model_migration" reqArguments

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
      runMigrationWithEmptyDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "state" structuredContent `shouldBe` Just (String "Conflict")
      liftIO $ (valueField "uuid" =<< valueField "targetPackage" structuredContent) `shouldBe` Just (toJSON netherlandsKmPackageV2.uuid)
      assertConflict structuredContent (0, 1) a_km1_ch4
      liftIO $ fmap (fmap (valueField "uuid")) (thirdPartyArray "before" structuredContent) `shouldBe` Just []
      liftIO $ fmap (take 1 . fmap (valueField "uuid")) (thirdPartyArray "after" structuredContent) `shouldBe` Just [Just (toJSON a_km1_ch4.entityUuid)]
      assertToolOutputSchema reqHeaders "start_knowledge_model_migration" structuredContent
      -- AND: Compare state in DB with expectation
      assertRunningKnowledgeModelMigrationInDB requestContext amsterdamKnowledgeModelEditorList.uuid netherlandsKmPackageV2.uuid
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
