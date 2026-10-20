module Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.DeleteKnowledgeModelEditor (
  delete_knowledge_model_editor,
) where

import Data.Aeson
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelEditorTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_knowledge_model_editor)
-- ------------------------------------------------------------------------
delete_knowledge_model_editor :: RequestContext -> SpecWith ((), Application)
delete_knowledge_model_editor requestContext =
  describe "POST /api/mcp (delete_knowledge_model_editor)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditor.uuid]

reqBody = toolCallBody "delete_knowledge_model_editor" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertKnowledgeModelEditorsInDB requestContext []
  it "HTTP 200 OK if the tools are listed (delete is destructive, save is not)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let annotations name = valueField "annotations" =<< L.find ((== Just (String name)) . valueField "name") (toolsOf response)
      liftIO $ annotations "delete_knowledge_model_editor" `shouldBe` Just (object ["readOnlyHint" .= False, "destructiveHint" .= True, "idempotentHint" .= True, "openWorldHint" .= False])
      liftIO $ annotations "save_knowledge_model_editor" `shouldBe` Just (object ["readOnlyHint" .= False, "destructiveHint" .= False, "idempotentHint" .= False, "openWorldHint" .= False])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
