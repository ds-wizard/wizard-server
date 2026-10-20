module Specs.Service.Mcp.Tool.WorkspaceTools.DeleteWorkspace (
  delete_workspace,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.RolePermission
import Shared.Model.Workspace.Workspace
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Service.Mcp.Tool.WorkspaceTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_workspace)
-- ------------------------------------------------------------------------
delete_workspace :: RequestContext -> SpecWith ((), Application)
delete_workspace requestContext =
  describe "POST /api/mcp (delete_workspace)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["workspace_uuid" .= secondWorkspace.uuid]

reqBody = toolCallBody "delete_workspace" reqArguments

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
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ fmap (L.isInfixOf "Second Workspace with 0 projects") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "delete_workspace" structuredContent
      -- AND: Compare state in DB with expectation
      assertWorkspaceInDB requestContext secondWorkspace.uuid True
  it "HTTP 200 OK if the confirmation token is given (the workspace deleted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      insertSecondWorkspace requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "delete_workspace" (object ["confirmation_token" .= token, "workspace_uuid" .= secondWorkspace.uuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertWorkspaceInDB requestContext secondWorkspace.uuid False
  it "HTTP 200 OK if it is the last workspace (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqArguments = object ["workspace_uuid" .= defaultWorkspaceUuid]
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__LAST_WORKSPACE)
      -- AND: Run migrations
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders (toolCallBody "delete_workspace" reqArguments)
      let reqBody = toolCallBody "delete_workspace" (object ["confirmation_token" .= token, "workspace_uuid" .= defaultWorkspaceUuid])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertWorkspaceInDB requestContext defaultWorkspaceUuid True
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _WORKSPACES_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
