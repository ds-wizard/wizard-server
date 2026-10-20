module Specs.Service.Mcp.Tool.OpenIdClientTools.DeleteOpenIdClient (
  delete_open_id_client,
) where

import Control.Monad (void)
import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified Shared.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.User.UserService (createUserFromOpenIdLogin)
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.OpenIdClient.Common
import Specs.Common
import Specs.Service.Mcp.Tool.OpenIdClientTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_open_id_client)
-- ------------------------------------------------------------------------
delete_open_id_client :: RequestContext -> SpecWith ((), Application)
delete_open_id_client requestContext =
  describe "POST /api/mcp (delete_open_id_client)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["open_id_client_uuid" .= defaultOpenIdClient.uuid]

reqBody = toolCallBody "delete_open_id_client" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfOpenIdClientInDB requestContext defaultOpenIdClient.uuid
  it "HTTP 200 OK if the client is the last login of the admins (tool error)" $
    -- GIVEN: Prepare expectation
    do
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
  it "HTTP 200 OK if the only admin who can log in was created by single sign-on (tool error)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN)
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration requestContext
      runInContextIO (void $ updateUserPasswordByUuid userAlbert.uuid "no-hash" (dt' 2024 1 1)) requestContext
      ssoUser <- getOneFromDB (createUserFromOpenIdLogin defaultOpenIdClient "ada" "Ada" "Lovelace" "ada@example.com" Nothing Nothing True Nothing) requestContext
      runInContextIO (void $ updateUserByUuid (ssoUser {role = toRoleSimple adminRole} :: User)) requestContext
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
