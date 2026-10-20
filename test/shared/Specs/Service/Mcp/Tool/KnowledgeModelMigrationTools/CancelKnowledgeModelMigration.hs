module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.CancelKnowledgeModelMigration (
  cancel_knowledge_model_migration,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.KnowledgeModelEditor.Migration.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (cancel_knowledge_model_migration)
-- ------------------------------------------------------------------------
cancel_knowledge_model_migration :: RequestContext -> SpecWith ((), Application)
cancel_knowledge_model_migration requestContext =
  describe "POST /api/mcp (cancel_knowledge_model_migration)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["knowledge_model_editor_uuid" .= amsterdamKnowledgeModelEditorList.uuid]

reqBody = toolCallBody "cancel_knowledge_model_migration" reqArguments

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
      runMigrationWithFullDB requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfKnowledgeModelMigrationInDB requestContext amsterdamKnowledgeModelEditorList.uuid
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
