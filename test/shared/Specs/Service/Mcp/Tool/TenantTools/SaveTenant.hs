module Specs.Service.Mcp.Tool.TenantTools.SaveTenant (
  save_tenant,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.TenantTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_tenant)
-- ------------------------------------------------------------------------
save_tenant :: RequestContext -> SpecWith ((), Application)
save_tenant requestContext =
  describe "POST /api/mcp (save_tenant)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object newTenantArguments

reqBody = toolCallBody "save_tenant" reqArguments

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
      liftIO $ valueField "tenantId" structuredContent `shouldBe` Just (String "mcp-tenant")
      assertToolOutputSchema reqHeaders "save_tenant" structuredContent
      -- AND: Compare state in DB with expectation
      assertTenantInDB requestContext newTenantUuid (Just ("mcp-tenant", "MCP Tenant"))
      assertTenantAdminsInDB requestContext newTenantUuid ["ada.lovelace@example.com"]
  it "HTTP 200 OK if the call is repeated (the tenant and its first admin created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      _ <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertTenantInDB requestContext newTenantUuid (Just ("mcp-tenant", "MCP Tenant"))
      assertTenantAdminsInDB requestContext newTenantUuid ["ada.lovelace@example.com"]
  it "HTTP 200 OK if the change is partial (the other fields kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_tenant" (object ["uuid" .= differentTenantUuid, "name" .= ("Renamed Tenant" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "name" =<< structuredContentOf response) `shouldBe` Just (String "Renamed Tenant")
      -- AND: Compare state in DB with expectation
      assertTenantInDB requestContext differentTenantUuid (Just ("different", "Renamed Tenant"))
  it "HTTP 200 OK if a new tenant has no first admin (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_tenant" (object (filter ((/= "admin") . fst) newTenantArguments))
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MISSING_ARGUMENTS "tenant" ["admin"])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertTenantInDB requestContext newTenantUuid Nothing
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
