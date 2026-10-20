module Specs.Service.Mcp.Tool.UserTools.UpdateUser (
  update_user,
) where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Localization.Messages.Mcp.Public
import Shared.Localization.Messages.Public
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Date
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (update_user)
-- ------------------------------------------------------------------------
update_user :: RequestContext -> SpecWith ((), Application)
update_user requestContext =
  describe "POST /api/mcp (update_user)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["user_uuid" .= userNikola.uuid, "firstName" .= ("Nik" :: String), "active" .= False]

reqBody = toolCallBody "update_user" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "update_user" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext (userNikola {firstName = "Nik", active = False} :: User)
  it "HTTP 200 OK if the role changes and the user has mcp.sensitive" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userNikola.uuid, "roleUuid" .= researcherRole.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext (userNikola {role = toRoleSimple researcherRole} :: User)
  it "HTTP 200 OK if the role changes and the user has no mcp.sensitive (tool error, the role kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userNikola.uuid, "roleUuid" .= adminRole.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext userNikola
  it "HTTP 200 OK if the e-mail changes and the user has no mcp.sensitive (tool error, the e-mail kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userNikola.uuid, "email" .= ("agent@example.com" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_VALIDATION__MISSING_PERMISSION _MCP_SENSITIVE_ROLE_PERMISSION)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      removeSensitivePermission requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext userNikola
  it "HTTP 200 OK if the user changed after expected_updated_at (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userNikola.uuid, "expected_updated_at" .= dt' 2017 1 1, "firstName" .= ("Nik" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_MCP__CHANGED_SINCE "user" (iso8601Show userNikola.updatedAt))
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the change deactivates the last active admin (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userAlbert.uuid, "active" .= False])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext userAlbert
  it "HTTP 200 OK if the role change leaves no active admin (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "update_user" (object ["user_uuid" .= userAlbert.uuid, "roleUuid" .= researcherRole.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_MCP__LAST_ADMIN_LOGIN)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext userAlbert

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
