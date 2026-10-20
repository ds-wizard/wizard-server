module Specs.Service.Mcp.Tool.ApiKeyTools.DeleteMcpConnection (
  delete_mcp_connection,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.User.RolePermission
import Shared.Model.User.UserToken
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.ApiKeyTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_mcp_connection)
-- ------------------------------------------------------------------------
delete_mcp_connection :: RequestContext -> SpecWith ((), Application)
delete_mcp_connection requestContext =
  describe "POST /api/mcp (delete_mcp_connection)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["connection_uuid" .= albertMcpToken.uuid]

reqBody = toolCallBody "delete_mcp_connection" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertUserTokensOfTypeInDB requestContext "McpUserTokenType" []
  it "HTTP 200 OK if the token is not one of your MCP connections (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_mcp_connection" (object ["connection_uuid" .= albertToken.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_DATABASE__ENTITY_NOT_FOUND "user_token" [("tenant_uuid", show defaultTenantUuid), ("uuid", show albertToken.uuid)])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertUserTokensOfTypeInDB requestContext "LoginUserTokenType" [albertToken]
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
