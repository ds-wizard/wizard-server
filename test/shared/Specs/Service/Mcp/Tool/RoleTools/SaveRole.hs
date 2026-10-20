module Specs.Service.Mcp.Tool.RoleTools.SaveRole (
  save_role,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Localization.Messages.Public
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Role.Common
import Specs.Service.Mcp.Tool.RoleTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_role)
-- ------------------------------------------------------------------------
save_role :: RequestContext -> SpecWith ((), Application)
save_role requestContext =
  describe "POST /api/mcp (save_role)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["uuid" .= newRoleUuid, "tenant" .= True, "name" .= ("Agent admin" :: String), "permissions" .= mcpPermissions]

reqBody = toolCallBody "save_role" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ (valueField "uuid" structuredContent, valueField "permissions" structuredContent) `shouldBe` (Just (toJSON newRoleUuid), Just (toJSON mcpPermissions))
      assertToolOutputSchema reqHeaders "save_role" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfRoleInDB requestContext newRole
  it "HTTP 200 OK if the call is repeated (the role created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ structuredContentOf response `shouldBe` structuredContentOf firstResponse
      -- AND: Compare state in DB with expectation
      assertExistenceOfRoleInDB requestContext newRole
  it "HTTP 200 OK if the change is partial (the name kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_role" (object ["uuid" .= dataStewardRole.uuid, "permissions" .= [_PROJECTS_CREATE_ROLE_PERMISSION]])
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
      assertExistenceOfRoleInDB requestContext (dataStewardRole {permissions = [_PROJECTS_CREATE_ROLE_PERMISSION]} :: Role)
  it "HTTP 200 OK if the role carries mcp.sensitive and the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfRoleInDB requestContext newRoleUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
