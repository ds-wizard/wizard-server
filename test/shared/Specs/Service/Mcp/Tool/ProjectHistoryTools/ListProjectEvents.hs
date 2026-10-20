module Specs.Service.Mcp.Tool.ProjectHistoryTools.ListProjectEvents (
  list_project_events,
) where

import Data.Aeson
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.Common.Lens
import Shared.Model.Project.Event.ProjectEventLenses ()
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_project_events)
-- ------------------------------------------------------------------------
list_project_events :: RequestContext -> SpecWith ((), Application)
list_project_events requestContext =
  describe "POST /api/mcp (list_project_events)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["size" .= (2 :: Int)]

reqBody = toolCallBody "list_project_events" reqArguments

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
      let Just (Array events) = valueField "projectEvents" (jsonField "_embedded" structuredContent)
      liftIO $ fmap (jsonField "uuid") (V.toList events) `shouldBe` fmap (toJSON . getUuid) (take 2 (reverse (fEvents project1.uuid)))
      liftIO $ valueField "thirdPartyContent" (jsonField "value" (V.head events)) `shouldSatisfy` (/= Nothing)
      assertToolOutputSchema reqHeaders "list_project_events" structuredContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
