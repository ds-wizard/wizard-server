module Specs.Service.Mcp.Tool.OperationTools.RetryPersistentCommand (
  retry_persistent_command,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Localization.Messages.Mcp.Public
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.OperationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (retry_persistent_command)
-- ------------------------------------------------------------------------
retry_persistent_command :: RequestContext -> SpecWith ((), Application)
retry_persistent_command requestContext =
  describe "POST /api/mcp (retry_persistent_command)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["persistent_command_uuid" .= failedCommand.uuid]

reqBody = toolCallBody "retry_persistent_command" reqArguments

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
      insertCommand requestContext failedCommand
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "uuid" structuredContent `shouldBe` Just (toJSON failedCommand.uuid)
      assertToolOutputSchema reqHeaders "retry_persistent_command" structuredContent
  it "HTTP 200 OK if the command is done (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "retry_persistent_command" (object ["persistent_command_uuid" .= doneCommand.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__PERSISTENT_COMMAND_DONE (U.toString doneCommand.uuid))
      -- AND: Run migrations
      insertCommand requestContext doneCommand
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfPersistentCommandInDB requestContext doneCommand
  createToolNoPermissionTest requestContext reqBody _DEV_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
