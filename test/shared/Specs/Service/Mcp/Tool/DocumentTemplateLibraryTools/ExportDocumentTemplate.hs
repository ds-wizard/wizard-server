module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ExportDocumentTemplate (
  export_document_template,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (export_document_template)
-- ------------------------------------------------------------------------
export_document_template :: RequestContext -> SpecWith ((), Application)
export_document_template requestContext =
  describe "POST /api/mcp (export_document_template)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["document_template_uuid" .= wizardDocumentTemplate.uuid]

reqBody = toolCallBody "export_document_template" reqArguments

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
      runTemplateMigrations requestContext
      runInContextIO DocumentTemplate.runS3Migration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap null (stringField "url" structuredContent) `shouldBe` Just False
      liftIO $ stringField "contentType" structuredContent `shouldBe` Just "application/zip"
      assertToolOutputSchema reqHeaders "export_document_template" structuredContent
  it "HTTP 200 OK if the template does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "export_document_template" (object ["document_template_uuid" .= unknownUuid])
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
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
