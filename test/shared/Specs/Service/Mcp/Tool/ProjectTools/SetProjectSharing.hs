module Specs.Service.Mcp.Tool.ProjectTools.SetProjectSharing (
  set_project_sharing,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Project.ProjectVisibilityJM ()
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (set_project_sharing)
-- ------------------------------------------------------------------------
set_project_sharing :: RequestContext -> SpecWith ((), Application)
set_project_sharing requestContext =
  describe "POST /api/mcp (set_project_sharing)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["visibility" .= VisibleViewProjectVisibility]

reqBody = toolCallBody "set_project_sharing" reqArguments

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
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "visibility" (jsonField "project" structuredContent) `shouldBe` toJSON VisibleViewProjectVisibility
      assertToolOutputSchema reqHeaders "set_project_sharing" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext (project1 {visibility = VisibleViewProjectVisibility})
  it "HTTP 200 OK if permissions are given (they replace the current ones)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_project_sharing" (projectArguments ["permissions" .= ([] :: [Value])])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (jsonField "permissions" <$> structuredContentOf response) `shouldBe` Just (toJSON ([] :: [Value]))
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext (project1 {permissions = []})
  it "HTTP 200 OK if the project changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_project_sharing" (projectArguments ["expected_updated_at" .= dt' 2018 1 1, "visibility" .= VisibleViewProjectVisibility])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__CHANGED_SINCE "project" (iso8601Show project1.updatedAt))
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext project1
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
