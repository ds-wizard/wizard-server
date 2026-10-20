module Specs.Service.Mcp.Tool.PrefabTools.ListIntegrationPrefabs (
  list_integration_prefabs,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Prefab.PrefabJM ()
import Shared.Database.Migration.Development.Prefab.Data.Prefabs
import qualified Shared.Database.Migration.Development.Prefab.PrefabMigration as Prefab
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_integration_prefabs)
-- ------------------------------------------------------------------------
list_integration_prefabs :: RequestContext -> SpecWith ((), Application)
list_integration_prefabs requestContext =
  describe "POST /api/mcp (list_integration_prefabs)" $ do
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

reqBody = toolCallBody "list_integration_prefabs" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expContent = toJSON (Page "prefabs" (PageMetadata 20 1 1 0) [kmIntegrationBioPortalPrefab])
      let expDto = toolResult expContent
      -- AND: Run migrations
      runInContextIO Prefab.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_integration_prefabs" expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
