module Specs.Service.Mcp.Tool.TenantTools.UpdateTenantLimits (
  update_tenant_limits,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.Tenant.Data.WizardTenantLimitBundles
import Shared.Model.Tenant.Limit.TenantLimitBundle
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.TenantTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (update_tenant_limits)
-- ------------------------------------------------------------------------
update_tenant_limits :: RequestContext -> SpecWith ((), Application)
update_tenant_limits requestContext =
  describe "POST /api/mcp (update_tenant_limits)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["tenant_uuid" .= differentTenantLimitBundle.uuid, "users" .= (77 :: Int)]

reqBody = toolCallBody "update_tenant_limits" reqArguments

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
      liftIO $ (valueField "max" =<< valueField "users" structuredContent) `shouldBe` Just (Number 77)
      assertToolOutputSchema reqHeaders "update_tenant_limits" structuredContent
      -- AND: Compare state in DB with expectation
      assertTenantLimitBundleInDB requestContext differentTenantLimitBundle {users = 77}
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION
  createToolNoPermissionTest requestContext reqBody _TENANTS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
