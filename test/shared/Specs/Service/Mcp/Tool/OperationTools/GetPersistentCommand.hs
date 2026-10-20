module Specs.Service.Mcp.Tool.OperationTools.GetPersistentCommand (
  get_persistent_command,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.OperationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (get_persistent_command)
-- ------------------------------------------------------------------------
get_persistent_command :: RequestContext -> SpecWith ((), Application)
get_persistent_command requestContext =
  describe "POST /api/mcp (get_persistent_command)" $ do
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

reqBody = toolCallBody "get_persistent_command" reqArguments

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
      liftIO $ valueField "state" structuredContent `shouldBe` Just (toJSON ErrorPersistentCommandState)
      assertToolOutputSchema reqHeaders "get_persistent_command" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfPersistentCommandInDB requestContext failedCommand
  createToolNoPermissionTest requestContext reqBody _DEV_USE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
