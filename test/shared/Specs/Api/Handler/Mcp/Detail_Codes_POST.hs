module Specs.Api.Handler.Mcp.Detail_Codes_POST (
  detail_codes_POST,
) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Code.McpCodeCreateJM ()
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.Migration.Development.Mcp.Data.McpAuthorizationCodes
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Error.Error
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp/oauth2/codes
-- ------------------------------------------------------------------------
detail_codes_POST :: RequestContext -> SpecWith ((), Application)
detail_codes_POST requestContext =
  describe "POST /api/mcp/oauth2/codes" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp/oauth2/codes"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = reqDtoWith Nothing Nothing

reqDtoWith :: Maybe String -> Maybe String -> McpCodeCreateDTO
reqDtoWith mChallenge mMethod = reqDtoFor mcpClient.clientId mChallenge mMethod Nothing

reqDtoFor :: String -> Maybe String -> Maybe String -> Maybe [McpToolset] -> McpCodeCreateDTO
reqDtoFor clientId mChallenge mMethod mToolsets =
  McpCodeCreateDTO {clientId = clientId, redirectUri = mcpRedirectUri, codeChallenge = mChallenge, codeChallengeMethod = mMethod, toolsets = mToolsets}

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqDto mcpAuthorizationCode
  create_test_200
    "HTTP 200 OK if the client uses PKCE"
    requestContext
    (reqDtoWith (Just mcpPkceChallenge) (Just "S256"))
    (mcpAuthorizationCode {codeChallenge = Just mcpPkceChallenge, codeChallengeMethod = Just "S256"} :: McpAuthorizationCode)
  create_test_200
    "HTTP 200 OK if the user grants only some toolsets"
    requestContext
    (reqDtoFor mcpClient.clientId Nothing Nothing (Just [KmMcpToolset, DtMcpToolset]))
    (mcpAuthorizationCode {toolsets = [KmMcpToolset, DtMcpToolset]} :: McpAuthorizationCode)

create_test_200 title requestContext reqDto expCode =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      let Just (String code) = valueField "code" =<< decode (simpleBody response)
      assertExistenceOfMcpAuthorizationCodeInDB requestContext (T.unpack code) expCode

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400
    "HTTP 400 BAD REQUEST if codeChallengeMethod is not S256"
    requestContext
    (reqDtoWith (Just mcpPkceChallenge) (Just "plain"))
    (UserError $ _ERROR_VALIDATION__MCP_CODE_CHALLENGE_METHOD "plain")
  create_test_400
    "HTTP 400 BAD REQUEST if codeChallenge has a wrong length"
    requestContext
    (reqDtoWith (Just "short") (Just "S256"))
    (UserError _ERROR_VALIDATION__MCP_CODE_CHALLENGE)
  it "HTTP 400 BAD REQUEST if a toolset is unknown" $
    -- GIVEN: Prepare request
    do
      let reqBody = encode $ object ["clientId" .= mcpClient.clientId, "redirectUri" .= mcpRedirectUri, "toolsets" .= ["km" :: String, "unknown"]]
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderUtf8 : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyContainsInvalidJsonMessage}
      response `shouldRespondWith` responseMatcher

create_test_400 title requestContext reqDto expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = do
  createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
  createMcpTokenTest requestContext reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    reqUrl
    reqHeaders
    (encode $ reqDtoFor "unknown" Nothing Nothing Nothing)
    "mcp_client"
    [("client_id", "unknown")]
