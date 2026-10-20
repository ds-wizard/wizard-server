module Specs.Service.Mcp.Tool.WorkspaceTools.SaveWorkspace (
  save_workspace,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Service.Mcp.Tool.WorkspaceTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_workspace)
-- ------------------------------------------------------------------------
save_workspace :: RequestContext -> SpecWith ((), Application)
save_workspace requestContext =
  describe "POST /api/mcp (save_workspace)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["uuid" .= newWorkspaceUuid, "name" .= ("Chemistry" :: String), "description" .= ("Chemistry projects" :: String)]

reqBody = toolCallBody "save_workspace" reqArguments

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
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "uuid" structuredContent `shouldBe` Just (toJSON newWorkspaceUuid)
      assertToolOutputSchema reqHeaders "save_workspace" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfWorkspaceInDB requestContext newWorkspace
      assertExistenceOfWorkspaceMembershipInDB requestContext newWorkspaceUuid userAlbert.uuid Nothing
  it "HTTP 200 OK if the call is repeated (the workspace created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldBe` structuredContentOf firstResponse
      -- AND: Compare state in DB with expectation
      assertExistenceOfWorkspaceInDB requestContext newWorkspace
  it "HTTP 200 OK if the change is partial (the other fields kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_workspace" (object ["uuid" .= defaultWorkspaceUuid, "description" .= ("Changed" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfWorkspaceInDB requestContext (defaultWorkspace {description = Just "Changed"} :: Workspace)
  it "HTTP 200 OK if the workspace changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_workspace" (object ["uuid" .= defaultWorkspaceUuid, "expected_updated_at" .= dt' 2017 1 1, "name" .= ("Changed" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__CHANGED_SINCE "workspace" (iso8601Show defaultWorkspace.updatedAt))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a new workspace has no name (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_workspace" (object ["description" .= ("Chemistry projects" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "workspace" ["name"])
      -- AND: Run migrations
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a new workspace is created in a single-workspace tenant (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_workspace" (object ["name" .= ("Chemistry" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
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
