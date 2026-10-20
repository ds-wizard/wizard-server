module Specs.Service.Mcp.Tool.OperationTools.ExecuteDevOperation (
  execute_dev_operation,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
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
import Specs.Service.Mcp.Tool.OperationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (execute_dev_operation)
-- ------------------------------------------------------------------------
execute_dev_operation :: RequestContext -> SpecWith ((), Application)
execute_dev_operation requestContext =
  describe "POST /api/mcp (execute_dev_operation)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object executeArguments

reqBody = toolCallBody "execute_dev_operation" reqArguments

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
      liftIO $ fmap (L.isInfixOf "Purge All Caches") (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "execute_dev_operation" structuredContent
  it "HTTP 200 OK if the confirmation token is given (the operation runs)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["output" .= ("Done" :: String)])
      -- AND: Run migrations
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "execute_dev_operation" (object (("confirmation_token" .= token) : executeArguments))
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "execute_dev_operation" structuredContent
  it "HTTP 200 OK if the operation does not exist (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "execute_dev_operation" (object ["section" .= ("Cache" :: String), "operation" .= ("Unknown" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__DEV_OPERATION_ABSENCE "Cache" "Unknown")
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if a parameter is missing (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody =
            toolCallBody
              "execute_dev_operation"
              (object ["section" .= ("Knowledge Model Editor" :: String), "operation" .= ("Squash Events for Knowledge Model Editor" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__DEV_OPERATION_PARAMETERS "Squash Events for Knowledge Model Editor" ["editorUuid"])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _DEV_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
