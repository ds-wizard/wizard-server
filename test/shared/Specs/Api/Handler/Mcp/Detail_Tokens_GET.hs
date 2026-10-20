module Specs.Api.Handler.Mcp.Detail_Tokens_GET (
  detail_tokens_GET,
) where

import Data.Aeson (toJSON)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.UserToken.UserTokenListJM ()
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Model.Cache.ServerCache
import Shared.Service.UserToken.UserTokenMapper (toList)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/mcp/tokens
-- ------------------------------------------------------------------------
detail_tokens_GET :: RequestContext -> SpecWith ((), Application)
detail_tokens_GET requestContext =
  describe "GET /api/mcp/tokens" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/mcp/tokens"

reqHeaders = [reqAuthHeader]

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
      let expDto = [toList albertMcpToken False]
      -- AND: Run migrations
      runInContextIO (insertUserToken albertMcpToken) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson (toJSON expDto)}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = do
  createAuthTest reqMethod reqUrl [] reqBody
  createMcpTokenTest requestContext reqMethod reqUrl [] reqBody
