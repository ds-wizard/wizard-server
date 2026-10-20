module Specs.Api.Handler.Mcp.Detail_Register_POST (
  detail_register_POST,
) where

import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateJM ()
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationJM ()
import Shared.Database.DAO.Mcp.McpClientDAO
import Shared.Database.Migration.Development.Mcp.Data.McpClients
import Shared.Model.Mcp.McpClient
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp/oauth2/register
-- ------------------------------------------------------------------------
detail_register_POST :: RequestContext -> SpecWith ((), Application)
detail_register_POST requestContext =
  describe "POST /api/mcp/oauth2/register" $ do
    test_201 requestContext
    test_400 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp/oauth2/register"

reqHeaders = [reqCtHeader]

reqDto = reqDtoWith Nothing

reqDtoWith :: Maybe String -> McpClientRegistrationCreateDTO
reqDtoWith = registrationDto [mcpRedirectUri] (Just "Claude")

registrationDto :: [String] -> Maybe String -> Maybe String -> McpClientRegistrationCreateDTO
registrationDto redirectUris clientName mMethod = McpClientRegistrationCreateDTO {redirectUris = redirectUris, clientName = clientName, tokenEndpointAuthMethod = mMethod}

reqBody = encode reqDto

loopbackRedirectUris :: [String]
loopbackRedirectUris = ["http://localhost:8080/callback", "http://127.0.0.1:52000/cb", "http://[::1]:52000/cb"]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 "HTTP 201 CREATED" requestContext (reqDtoWith Nothing) (mcpClient {redirectUris = [mcpRedirectUri]})
  create_test_201
    "HTTP 201 CREATED if the client asks for client_secret_basic"
    requestContext
    (reqDtoWith (Just "client_secret_basic"))
    (mcpClient {tokenEndpointAuthMethod = "client_secret_basic"})
  create_test_201 "HTTP 201 CREATED if the client asks for none (no secret)" requestContext (reqDtoWith (Just "none")) (mcpClient {tokenEndpointAuthMethod = "none"})
  create_test_201
    "HTTP 201 CREATED if the redirect uris are http on a loopback host"
    requestContext
    (registrationDto loopbackRedirectUris (Just "Claude") Nothing)
    (mcpClient {redirectUris = loopbackRedirectUris})

create_test_201 title requestContext reqDto expClient =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeader : resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let Just (String clientId) = valueField "client_id" =<< decode (simpleBody response)
      clientFromDb <- getOneFromDB (findMcpClientByClientId (T.unpack clientId)) requestContext
      let expDto =
            McpClientRegistrationDTO
              { clientId = clientFromDb.clientId
              , clientSecret = if expClient.tokenEndpointAuthMethod == "none" then Nothing else Just clientFromDb.clientSecret
              , clientIdIssuedAt = floor . utcTimeToPOSIXSeconds $ clientFromDb.createdAt
              , clientSecretExpiresAt = 0
              , redirectUris = expClient.redirectUris
              , clientName = expClient.clientName
              , tokenEndpointAuthMethod = expClient.tokenEndpointAuthMethod
              }
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson (toJSON expDto)}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfMcpClientInDB requestContext (T.unpack clientId) expClient

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400
    "HTTP 400 BAD REQUEST if token_endpoint_auth_method is unknown"
    (reqDtoWith (Just "private_key_jwt"))
    (oauthError "invalid_client_metadata" "Unsupported token_endpoint_auth_method: private_key_jwt")
  create_test_400
    "HTTP 400 BAD REQUEST if client_name is too long"
    (registrationDto [mcpRedirectUri] (Just (replicate 256 'x')) Nothing)
    (oauthError "invalid_client_metadata" "client_name is too long")
  create_test_400
    "HTTP 400 BAD REQUEST if redirect_uris is empty"
    (registrationDto [] (Just "Claude") Nothing)
    (oauthError "invalid_redirect_uri" "At least one redirect_uri is required")
  create_test_400
    "HTTP 400 BAD REQUEST if there are too many redirect_uris"
    (registrationDto (replicate 11 mcpRedirectUri) (Just "Claude") Nothing)
    (oauthError "invalid_redirect_uri" "At most 10 redirect_uris are allowed")
  mapM_
    ( \redirectUri ->
        create_test_400
          ("HTTP 400 BAD REQUEST if a redirect_uri is " ++ redirectUri)
          (registrationDto [redirectUri] (Just "Claude") Nothing)
          (oauthError "invalid_redirect_uri" ("Invalid redirect_uri: " ++ redirectUri))
    )
    ["javascript:alert(1)", "data:text/html,x", "http://attacker.example/cb", "https://claude.ai/cb#fragment", "/relative/cb", "not a uri"]

create_test_400 title reqDto expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
