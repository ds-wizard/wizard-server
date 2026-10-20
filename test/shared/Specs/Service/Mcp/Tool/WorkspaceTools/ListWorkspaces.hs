module Specs.Service.Mcp.Tool.WorkspaceTools.ListWorkspaces (
  list_workspaces,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_workspaces)
-- ------------------------------------------------------------------------
list_workspaces :: RequestContext -> SpecWith ((), Application)
list_workspaces requestContext =
  describe "POST /api/mcp (list_workspaces)" $ do
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

reqBody = toolCallBody "list_workspaces" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqArguments [defaultWorkspace, secondWorkspace]
  create_test_200 "HTTP 200 OK if a query is given" requestContext (object ["q" .= ("Second" :: String)]) [secondWorkspace]

create_test_200 title requestContext reqArguments expWorkspaces =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_workspaces" reqArguments
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON (Page "workspaces" (PageMetadata 20 (length expWorkspaces) 1 0) expWorkspaces)
      let expDto = toolResult expContent
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_workspaces" expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
