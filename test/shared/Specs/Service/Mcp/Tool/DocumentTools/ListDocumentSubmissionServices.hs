module Specs.Service.Mcp.Tool.DocumentTools.ListDocumentSubmissionServices (
  list_document_submission_services,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import Shared.Model.User.RolePermission
import Shared.Service.Submission.SubmissionService (toSubmissionServiceSimple)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.DocumentTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_document_submission_services)
-- ------------------------------------------------------------------------
list_document_submission_services :: RequestContext -> SpecWith ((), Application)
list_document_submission_services requestContext =
  describe "POST /api/mcp (list_document_submission_services)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = documentArguments []

reqBody = toolCallBody "list_document_submission_services" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = object ["services" .= [toSubmissionServiceSimple settingsSubmissionService]]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runDocumentMigrations requestContext
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_document_submission_services" expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
