module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.GetDocumentTemplate (
  get_document_template,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_document_template)
-- ------------------------------------------------------------------------
get_document_template :: RequestContext -> SpecWith ((), Application)
get_document_template requestContext =
  describe "POST /api/mcp (get_document_template)" $ do
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

reqBody = toolCallBody "get_document_template" reqArguments

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "documentTemplate" structuredContent `shouldBe` Just (markReadme wizardDocumentTemplateDetailDTO)
      liftIO $ fmap isObject (valueField "dependents" structuredContent) `shouldBe` Just True
      liftIO $ fmap isArray (valueField "files" structuredContent) `shouldBe` Just True
      assertToolOutputSchema reqHeaders "get_document_template" structuredContent
  it "HTTP 200 OK if the user has no documentTemplates.manage and no documentTemplates.useEditor (no dependents, no files)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runTemplateMigrations requestContext
      removeAlbertPermissions requestContext [_DOCUMENT_TEMPLATES_MANAGE_ROLE_PERMISSION, _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "dependents" structuredContent `shouldBe` Just Null
      liftIO $ valueField "files" structuredContent `shouldBe` Just Null
  it "HTTP 200 OK if the template does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "get_document_template" (object ["document_template_uuid" .= unknownUuid])
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
