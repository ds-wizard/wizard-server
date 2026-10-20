module Specs.Api.Handler.Mcp.Detail_OpenIdConfiguration_GET (
  detail_openIdConfiguration_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Mcp.Common

-- ------------------------------------------------------------------------
-- GET /api/mcp/.well-known/openid-configuration
-- ------------------------------------------------------------------------
detail_openIdConfiguration_GET :: RequestContext -> SpecWith ((), Application)
detail_openIdConfiguration_GET requestContext = do
  describe "GET /api/mcp/.well-known/openid-configuration" $
    test_200 requestContext "/api/mcp/.well-known/openid-configuration"
  describe "GET /.well-known/openid-configuration/api/mcp" $
    test_200 requestContext "/.well-known/openid-configuration/api/mcp"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext reqUrl =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = authorizationServerMetadata
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
