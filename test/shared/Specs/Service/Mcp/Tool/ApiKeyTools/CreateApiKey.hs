module Specs.Service.Mcp.Tool.ApiKeyTools.CreateApiKey (
  create_api_key,
) where

import Data.Aeson
import Data.Time
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Localization.Messages.Mcp.Public
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ApiKeyTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (create_api_key)
-- ------------------------------------------------------------------------
create_api_key :: RequestContext -> SpecWith ((), Application)
create_api_key requestContext =
  describe "POST /api/mcp (create_api_key)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["name" .= ("Agent key" :: String)]

reqBody = toolCallBody "create_api_key" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      expiresAt <- liftIO $ addUTCTime (10 * nominalDay) <$> getCurrentTime
      let reqBody = toolCallBody "create_api_key" (object ["name" .= ("Agent key" :: String), "expires_at" .= expiresAt])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      let Just token = stringField "token" structuredContent
      liftIO $ stringField "name" structuredContent `shouldBe` Just "Agent key"
      assertToolOutputSchema reqHeaders "create_api_key" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfApiKeyInDB requestContext "Agent key" token
  it "HTTP 200 OK if the expiration is too far in the future (tool error)" $
    -- GIVEN: Prepare request
    do
      expiresAt <- liftIO $ addUTCTime (365 * nominalDay) <$> getCurrentTime
      let reqBody = toolCallBody "create_api_key" (object ["name" .= ("Agent key" :: String), "expires_at" .= expiresAt])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__API_KEY_EXPIRATION 90)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertUserTokensOfTypeInDB requestContext "ApiKeyUserTokenType" []
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
