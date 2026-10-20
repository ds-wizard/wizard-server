module Specs.Api.Handler.Mcp.Detail_Clients_GET (
  detail_clients_GET,
) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Model.Mcp.McpClient
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/mcp/oauth2/clients/{clientId}
-- ------------------------------------------------------------------------
detail_clients_GET :: RequestContext -> SpecWith ((), Application)
detail_clients_GET requestContext =
  describe "GET /api/mcp/oauth2/clients/{clientId}" $ do
    test_200 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/api/mcp/oauth2/clients/" ++ mcpClient.clientId

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
      let expDto = object ["clientName" .= mcpClient.clientName, "redirectUris" .= mcpClient.redirectUris]
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  it "HTTP 404 NOT FOUND if the client does not exist" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_client" "Client 'audit-log-client' does not exist"
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
