module Specs.Service.Mcp.Tool.OpenIdClientTools.SaveOpenIdClient (
  save_open_id_client,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.User.RolePermission
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.OpenIdClient.Common
import Specs.Common
import Specs.Service.Mcp.Tool.OpenIdClientTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_open_id_client)
-- ------------------------------------------------------------------------
save_open_id_client :: RequestContext -> SpecWith ((), Application)
save_open_id_client requestContext =
  describe "POST /api/mcp (save_open_id_client)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments =
  object
    [ "uuid" .= newClientUuid
    , "name" .= ("Keycloak" :: String)
    , "url" .= ("https://keycloak.example.com" :: String)
    , "clientId" .= ("wizard" :: String)
    , "clientSecret" .= ("k3ycl0ak" :: String)
    ]

reqBody = toolCallBody "save_open_id_client" reqArguments

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
      liftIO $ (valueField "name" structuredContent, valueField "clientSecret" structuredContent) `shouldBe` (Just (String "Keycloak"), Just (String "********"))
      assertToolOutputSchema reqHeaders "save_open_id_client" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfOpenIdClientInDB requestContext newClient
  it "HTTP 200 OK if the masked secret is sent back (the stored secret kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_open_id_client" (object ["uuid" .= defaultOpenIdClient.uuid, "name" .= ("Renamed" :: String), "clientSecret" .= ("********" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfOpenIdClientInDB requestContext (defaultOpenIdClient {name = "Renamed"} :: OpenIdClient)
  it "HTTP 200 OK if the user has no mcp.sensitive (tool error, nothing stored)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfOpenIdClientInDB requestContext newClientUuid
  it "HTTP 200 OK if the url of the last login of the admins changes (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_open_id_client" (object ["uuid" .= defaultOpenIdClient.uuid, "url" .= ("https://idp.example.com" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN)
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration requestContext
      runAlbertOpenIdLoginOnly requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfOpenIdClientInDB requestContext defaultOpenIdClient
  it "HTTP 200 OK if a masked secret comes with a new url (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_open_id_client" (object ["uuid" .= defaultOpenIdClient.uuid, "url" .= ("https://idp.example.com" :: String), "clientSecret" .= ("********" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__MASKED_SECRET_TARGET_CHANGED "url")
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfOpenIdClientInDB requestContext defaultOpenIdClient

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
