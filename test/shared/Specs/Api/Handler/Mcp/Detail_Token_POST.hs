module Specs.Api.Handler.Mcp.Detail_Token_POST (
  detail_token_POST,
) where

import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Mcp.Token.McpTokenDTO
import Shared.Api.Resource.Mcp.Token.McpTokenJM ()
import Shared.Database.DAO.Mcp.McpAuthorizationCodeDAO
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.DAO.Mcp.McpRefreshTokenDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.Migration.Development.Mcp.Data.McpAuthorizationCodes
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Database.Migration.Development.Mcp.Data.McpRefreshTokens
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Mcp.McpAuthorizationCode
import Shared.Model.Mcp.McpClient
import Shared.Model.Mcp.McpRefreshToken
import Shared.Model.User.RolePermission
import Shared.Model.User.UserToken
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp/oauth2/token
-- ------------------------------------------------------------------------
detail_token_POST :: RequestContext -> SpecWith ((), Application)
detail_token_POST requestContext =
  describe "POST /api/mcp/oauth2/token" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp/oauth2/token"

reqHeaders = [reqFormHeader]

reqBody = formBody (codeGrant mcpAuthorizationCodeValue ++ clientCredentials)

codeGrant :: String -> [(String, String)]
codeGrant code = [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", mcpRedirectUri)]

clientCredentials :: [(String, String)]
clientCredentials = [("client_id", mcpClient.clientId), ("client_secret", mcpClient.clientSecret)]

refreshGrant :: String -> [(String, String)]
refreshGrant token = [("grant_type", "refresh_token"), ("refresh_token", token)] ++ clientCredentials

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext mcpClient mcpAuthorizationCode reqHeaders reqBody
  create_test_200
    "HTTP 200 OK if the code was issued with PKCE"
    requestContext
    mcpClient
    mcpPkceAuthorizationCode
    reqHeaders
    (formBody $ codeGrant mcpPkceCodeValue ++ clientCredentials ++ [("code_verifier", mcpPkceVerifier)])
  create_test_200
    "HTTP 200 OK if the code grants only some toolsets"
    requestContext
    mcpClient
    mcpToolsetsAuthorizationCode
    reqHeaders
    (formBody $ codeGrant mcpToolsetsCodeValue ++ clientCredentials)
  create_test_200
    "HTTP 200 OK if the client authenticates with client_secret_basic"
    requestContext
    mcpClient
    mcpAuthorizationCode
    (basicAuthHeader : reqHeaders)
    (formBody $ codeGrant mcpAuthorizationCodeValue)
  create_test_200
    "HTTP 200 OK if a public client uses PKCE"
    requestContext
    mcpPublicClient
    mcpPublicPkceAuthorizationCode
    reqHeaders
    (formBody $ codeGrant mcpPublicPkceCodeValue ++ [("client_id", mcpPublicClient.clientId), ("code_verifier", mcpPkceVerifier)])
  it "HTTP 200 OK if a refresh_token rotates the grant" $
    -- GIVEN: Prepare request
    do
      let reqBody = formBody (refreshGrant mcpRefreshTokenValue)
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runMcpGrantMigration requestContext mcpGrantRefreshToken
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let Just (String accessToken) = valueField "access_token" =<< decode (simpleBody response)
      let Just (String refreshToken) = valueField "refresh_token" =<< decode (simpleBody response)
      let expDto = McpTokenDTO {accessToken = T.unpack accessToken, tokenType = "Bearer", expiresIn = 3600, refreshToken = T.unpack refreshToken}
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson (toJSON expDto)}
      response `shouldRespondWith` responseMatcher
      liftIO $ expDto.refreshToken `shouldNotBe` mcpRefreshTokenValue
      liftIO $ length expDto.refreshToken `shouldBe` 256
      -- AND: Compare state in DB with expectation
      userToken <- assertMcpGrantTokenInDB requestContext expDto.accessToken
      liftIO $ userToken.uuid `shouldBe` mcpGrantToken.uuid
      assertAbsenceOfMcpRefreshTokenInDB requestContext mcpRefreshTokenHash
      assertExistenceOfMcpRefreshTokenInDB requestContext expDto.refreshToken (mcpGrantRefreshToken {previousTokenHash = Just mcpRefreshTokenHash})

create_test_200 title requestContext client authCode headers body =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO (insertMcpClient client) requestContext
      runInContextIO (insertMcpAuthorizationCode authCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl headers body
      -- THEN: Compare response with expectation
      let Just (String accessToken) = valueField "access_token" =<< decode (simpleBody response)
      let Just (String refreshToken) = valueField "refresh_token" =<< decode (simpleBody response)
      let expDto = McpTokenDTO {accessToken = T.unpack accessToken, tokenType = "Bearer", expiresIn = 3600, refreshToken = T.unpack refreshToken}
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson (toJSON expDto)}
      response `shouldRespondWith` responseMatcher
      liftIO $ length expDto.refreshToken `shouldBe` 256
      -- AND: Compare state in DB with expectation
      userToken <- assertMcpGrantTokenInDB requestContext expDto.accessToken
      assertExistenceOfMcpRefreshTokenInDB
        requestContext
        expDto.refreshToken
        (mcpGrantRefreshToken {userTokenUuid = userToken.uuid, clientId = client.clientId, toolsets = authCode.toolsets})
      assertAbsenceOfMcpAuthorizationCodeInDB requestContext authCode.code

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400
    "HTTP 400 BAD REQUEST if code_verifier does not match"
    requestContext
    mcpPkceAuthorizationCode
    (formBody $ codeGrant mcpPkceCodeValue ++ clientCredentials ++ [("code_verifier", "wrong-verifier-wrong-verifier-wrong-verifier-wrong")])
    (oauthError "invalid_grant" "Invalid code_verifier")
  create_test_400
    "HTTP 400 BAD REQUEST if code_verifier has a wrong format"
    requestContext
    mcpPkceAuthorizationCode
    (formBody $ codeGrant mcpPkceCodeValue ++ clientCredentials ++ [("code_verifier", "short")])
    (oauthError "invalid_request" "Invalid code_verifier format")
  create_test_400
    "HTTP 400 BAD REQUEST if code_verifier is missing"
    requestContext
    mcpPkceAuthorizationCode
    (formBody $ codeGrant mcpPkceCodeValue ++ clientCredentials)
    (oauthError "invalid_request" "Missing code_verifier")
  create_test_400
    "HTTP 400 BAD REQUEST if the authorization code expired"
    requestContext
    mcpExpiredAuthorizationCode
    (formBody $ codeGrant mcpExpiredCodeValue ++ clientCredentials)
    (oauthError "invalid_grant" "Authorization code expired")
  create_test_400
    "HTTP 400 BAD REQUEST if redirect_uri does not match"
    requestContext
    mcpAuthorizationCode
    (formBody $ [("grant_type", "authorization_code"), ("code", mcpAuthorizationCodeValue), ("redirect_uri", "https://other.example/cb")] ++ clientCredentials)
    (oauthError "invalid_grant" "redirect_uri mismatch")
  create_test_400
    "HTTP 400 BAD REQUEST if the authorization code is unknown"
    requestContext
    mcpAuthorizationCode
    (formBody $ codeGrant "unknown-code" ++ clientCredentials)
    (oauthError "invalid_grant" "Invalid authorization code")
  create_test_400
    "HTTP 400 BAD REQUEST if code is missing"
    requestContext
    mcpAuthorizationCode
    (formBody $ ("grant_type", "authorization_code") : clientCredentials)
    (oauthError "invalid_request" "Missing code")
  create_test_400
    "HTTP 400 BAD REQUEST if grant_type is missing"
    requestContext
    mcpAuthorizationCode
    (formBody clientCredentials)
    (oauthError "invalid_request" "Missing grant_type")
  create_test_400
    "HTTP 400 BAD REQUEST if grant_type is unsupported"
    requestContext
    mcpAuthorizationCode
    (formBody $ ("grant_type", "password") : clientCredentials)
    (oauthError "unsupported_grant_type" "Unsupported grant_type: password")
  it "HTTP 400 BAD REQUEST if client_id does not match the Authorization header" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [basicAuthHeader, reqFormHeader]
      let reqBody = formBody $ codeGrant mcpAuthorizationCodeValue ++ [("client_id", "other-client")]
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_request" "client_id does not match the Authorization header"
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      runInContextIO (insertMcpAuthorizationCode mcpAuthorizationCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST if a public client has no PKCE" $
    -- GIVEN: Prepare request
    do
      let reqBody = formBody $ codeGrant mcpPublicCodeValue ++ [("client_id", mcpPublicClient.clientId)]
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_request" "A public client must use PKCE"
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpPublicClient) requestContext
      runInContextIO (insertMcpAuthorizationCode mcpPublicAuthorizationCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST if the user lacks mcp.read for authorization_code" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode missingMcpReadError
      -- AND: Run migrations
      runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _MCP_READ_ROLE_PERMISSION)) requestContext
      runInContextIO (insertMcpClient mcpClient) requestContext
      runInContextIO (insertMcpAuthorizationCode mcpAuthorizationCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST if the user lacks mcp.read for refresh_token" $
    -- GIVEN: Prepare request
    do
      let reqBody = formBody $ refreshGrant mcpRefreshTokenValue
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode missingMcpReadError
      -- AND: Run migrations
      runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _MCP_READ_ROLE_PERMISSION)) requestContext
      runMcpGrantMigration requestContext mcpGrantRefreshToken
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  create_test_400_refresh
    "HTTP 400 BAD REQUEST if refresh_token is missing"
    requestContext
    mcpGrantRefreshToken
    (formBody $ ("grant_type", "refresh_token") : clientCredentials)
    (oauthError "invalid_request" "Missing refresh_token")
  create_test_400_refresh
    "HTTP 400 BAD REQUEST if the refresh token expired"
    requestContext
    mcpExpiredGrantRefreshToken
    (formBody $ refreshGrant mcpRefreshTokenValue)
    (oauthError "invalid_grant" "Refresh token expired")
  it "HTTP 400 BAD REQUEST if the refresh token belongs to another client" $
    -- GIVEN: Prepare request
    do
      let reqBody = formBody [("grant_type", "refresh_token"), ("refresh_token", mcpRefreshTokenValue), ("client_id", mcpPublicClient.clientId)]
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_grant" "Refresh token does not belong to the client"
      -- AND: Run migrations
      runMcpGrantMigration requestContext mcpGrantRefreshToken
      runInContextIO (insertMcpClient mcpPublicClient) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 400 BAD REQUEST if a rotated refresh token is reused" $
    -- GIVEN: Prepare request
    do
      let reqBody = formBody (refreshGrant mcpRefreshTokenValue)
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_grant" "Invalid refresh token"
      -- AND: Run migrations
      runMcpGrantMigration requestContext mcpRotatedRefreshToken
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfUserTokenInDB requestContext mcpGrantToken
      assertAbsenceOfMcpRefreshTokenInDB requestContext mcpRotatedRefreshToken.tokenHash

missingMcpReadError = oauthError "invalid_grant" (localeMessage $ _ERROR_VALIDATION__MCP_MISSING_PERMISSION _MCP_READ_ROLE_PERMISSION)

create_test_400 title requestContext authCode reqBody expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      runInContextIO (insertMcpAuthorizationCode authCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

create_test_400_refresh title requestContext refreshTokenRow reqBody expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runMcpGrantMigration requestContext refreshTokenRow
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
  create_test_401
    "HTTP 401 UNAUTHORIZED if client_secret is wrong"
    requestContext
    (formBody $ codeGrant mcpAuthorizationCodeValue ++ [("client_id", mcpClient.clientId), ("client_secret", "wrong-secret")])
    (oauthError "invalid_client" "Invalid client credentials")
  create_test_401
    "HTTP 401 UNAUTHORIZED if client_id is unknown"
    requestContext
    (formBody $ codeGrant mcpAuthorizationCodeValue ++ [("client_id", "unknown-client"), ("client_secret", "secret")])
    (oauthError "invalid_client" "Invalid client credentials")
  create_test_401
    "HTTP 401 UNAUTHORIZED if client_id is missing"
    requestContext
    (formBody $ codeGrant mcpAuthorizationCodeValue)
    (oauthError "invalid_client" "Missing client_id")
  it "HTTP 401 UNAUTHORIZED if the Authorization header is malformed (Basic challenge)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [("Authorization", "Basic bm8tY29sb24="), reqFormHeader]
      let reqBody = formBody $ codeGrant mcpAuthorizationCodeValue
      -- AND: Prepare expectation
      let expStatus = 401
      let expHeaders = ("WWW-Authenticate" <:> "Basic realm=\"mcp\"") : resCtHeader : resCorsHeaders
      let expBody = encode $ oauthError "invalid_client" "Invalid Authorization header"
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      runInContextIO (insertMcpAuthorizationCode mcpAuthorizationCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

create_test_401 title requestContext reqBody expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 401
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertMcpClient mcpClient) requestContext
      runInContextIO (insertMcpAuthorizationCode mcpAuthorizationCode) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
runMcpGrantMigration requestContext refreshTokenRow = do
  runInContextIO (insertMcpClient mcpClient) requestContext
  runInContextIO (insertUserToken mcpGrantToken) requestContext
  runInContextIO (insertMcpRefreshToken refreshTokenRow) requestContext
