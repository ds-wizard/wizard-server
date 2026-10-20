module Specs.Service.Mcp.Tool.DocumentTemplateDraftTools.ListDocumentTemplateDrafts (
  list_document_template_drafts,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.User.RolePermission
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_document_template_drafts)
-- ------------------------------------------------------------------------
list_document_template_drafts :: RequestContext -> SpecWith ((), Application)
list_document_template_drafts requestContext =
  describe "POST /api/mcp (list_document_template_drafts)" $ do
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

reqBody = toolCallBody "list_document_template_drafts" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON $ Page "documentTemplateDrafts" (PageMetadata 20 1 1 0) [toDraftList wizardDocumentTemplateDraft]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_document_template_drafts" expContent
  it "HTTP 200 OK if the query matches no draft" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_document_template_drafts" (object ["q" .= ("nothing-like-this" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult . toJSON $ Page "documentTemplateDrafts" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateDraftList])
      -- AND: Run migrations
      runInContextIO DocumentTemplate.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
