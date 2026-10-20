module Specs.Service.Mcp.Tool.ProjectTools.UpdateProjectSettings (
  update_project_settings,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
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
-- POST /api/mcp (update_project_settings)
-- ------------------------------------------------------------------------
update_project_settings :: RequestContext -> SpecWith ((), Application)
update_project_settings requestContext =
  describe "POST /api/mcp (update_project_settings)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["name" .= ("Renamed" :: String)]

reqBody = toolCallBody "update_project_settings" reqArguments

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
      liftIO $ jsonField "name" structuredContent `shouldBe` String "Renamed"
      assertToolOutputSchema reqHeaders "update_project_settings" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext (project1 {name = "Renamed"})
  it "HTTP 200 OK if isTemplate is given" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_project_settings" (projectArguments ["isTemplate" .= False])
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
      liftIO $ (jsonField "isTemplate" <$> structuredContentOf response) `shouldBe` Just (Bool False)
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext (project1 {isTemplate = False})
  it "HTTP 200 OK if the project is a template and the user has no projects.manageTemplates (tool error, nothing stored)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_project_settings" (projectArguments ["isTemplate" .= False])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION)
      -- AND: Run migrations
      runProjectMigrations requestContext
      removeAlbertPermissions requestContext [_PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectInDB requestContext project1
  it "HTTP 200 OK if the project changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_project_settings" (projectArguments ["expected_updated_at" .= dt' 2018 1 1, "name" .= ("Renamed" :: String)])
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
