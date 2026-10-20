module Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.ListDocumentTemplates (
  list_document_templates,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Api.Resource.DocumentTemplate.WizardDocumentTemplateSimpleJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Service.Mcp.Tool.DocumentTemplateLibraryTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_document_templates)
-- ------------------------------------------------------------------------
list_document_templates :: RequestContext -> SpecWith ((), Application)
list_document_templates requestContext =
  describe "POST /api/mcp (list_document_templates)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object []

reqBody = toolCallBody "list_document_templates" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON $ Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runTemplateMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_document_templates" expContent
  it "HTTP 200 OK if the id is unknown (no templates)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_document_templates" (object ["id" .= ("non-existing-template" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult . toJSON $ Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO])
      -- AND: Run migrations
      runTemplateMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
