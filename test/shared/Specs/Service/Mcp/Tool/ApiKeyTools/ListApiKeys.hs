module Specs.Service.Mcp.Tool.ApiKeyTools.ListApiKeys (
  list_api_keys,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Model.Cache.ServerCache
import Shared.Model.User.RolePermission
import Shared.Model.User.UserToken
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_api_keys)
-- ------------------------------------------------------------------------
list_api_keys :: RequestContext -> SpecWith ((), Application)
list_api_keys requestContext =
  describe "POST /api/mcp (list_api_keys)" $ do
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

reqBody = toolCallBody "list_api_keys" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent =
            object
              [ "apiKeys"
                  .= [ object
                         [ "uuid" .= albertApiKey.uuid
                         , "name" .= albertApiKey.name
                         , "userAgent" .= albertApiKey.userAgent
                         , "currentSession" .= False
                         , "expiresAt" .= albertApiKey.expiresAt
                         , "createdAt" .= albertApiKey.createdAt
                         ]
                     ]
              ]
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO (insertUserToken albertApiKey) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_api_keys" expContent
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
