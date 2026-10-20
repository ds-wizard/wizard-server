module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.DeleteDocumentTemplateDraft (
  delete_document_template_draft,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.DocumentTemplate.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_document_template_draft)
-- ------------------------------------------------------------------------
delete_document_template_draft :: RequestContext -> SpecWith ((), Application)
delete_document_template_draft requestContext =
  describe "POST /api/mcp (delete_document_template_draft)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplateDraft.uuid]

reqBody = toolCallBody "delete_document_template_draft" reqArguments

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
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertCountInDB findDrafts requestContext 0
  it "HTTP 200 OK if the template is released (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_document_template_draft" (object ["document_template_uuid" .= wizardDocumentTemplate.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Just (Bool True)
      -- AND: Compare state in DB with expectation
      assertDocumentTemplatePhaseInDB requestContext wizardDocumentTemplate.uuid ReleasedDocumentTemplatePhase
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
