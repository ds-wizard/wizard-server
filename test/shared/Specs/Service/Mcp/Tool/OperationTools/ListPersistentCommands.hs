module Specs.Service.Mcp.Tool.OperationTools.ListPersistentCommands (
  list_persistent_commands,
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
import Specs.Service.Mcp.Tool.UserTools.Common (embeddedField)

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_persistent_commands)
-- ------------------------------------------------------------------------
list_persistent_commands :: RequestContext -> SpecWith ((), Application)
list_persistent_commands requestContext =
  describe "POST /api/mcp (list_persistent_commands)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["states" .= ["ErrorPersistentCommandState" :: String]]

reqBody = toolCallBody "list_persistent_commands" reqArguments

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
      liftIO $ embeddedField "persistentCommands" "uuid" structuredContent `shouldContain` [toJSON failedCommand.uuid]
      assertToolOutputSchema reqHeaders "list_persistent_commands" structuredContent
  createToolNoPermissionTest requestContext reqBody _DEV_USE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
