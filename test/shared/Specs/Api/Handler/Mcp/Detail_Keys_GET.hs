module Specs.Api.Handler.Mcp.Detail_Keys_GET (
  detail_keys_GET,
) where

import Data.Aeson (toJSON)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Model.Config.WizardServerConfig
import Shared.Util.Jwk
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp

-- ------------------------------------------------------------------------
-- GET /api/mcp/oauth2/keys
-- ------------------------------------------------------------------------
detail_keys_GET :: RequestContext -> SpecWith ((), Application)
detail_keys_GET requestContext =
  describe "GET /api/mcp/oauth2/keys" $
    test_200 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mcp/oauth2/keys"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toJwkSet requestContext.serverConfig.general.rsaPrivateKey
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson (toJSON expDto)}
      response `shouldRespondWith` responseMatcher
