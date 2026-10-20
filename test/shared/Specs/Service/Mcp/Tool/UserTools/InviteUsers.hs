module Specs.Service.Mcp.Tool.UserTools.InviteUsers (
  invite_users,
) where

import Data.Aeson
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.User.Data.Roles
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Localization.Messages.Public
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.User.RoleMapper (toRoleSimple)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Common
import Specs.Service.Mcp.Tool.UserTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (invite_users)
-- ------------------------------------------------------------------------
invite_users :: RequestContext -> SpecWith ((), Application)
invite_users requestContext =
  describe "POST /api/mcp (invite_users)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqUser = object ["firstName" .= ("John" :: String), "lastName" .= ("Doe" :: String), "email" .= ("john.doe@example.com" :: String)]

reqArguments = object ["users" .= [reqUser]]

reqBody = toolCallBody "invite_users" reqArguments

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
      liftIO $ stringField "summary" structuredContent `shouldBe` Just "Invites 1 users, each gets an e-mail with a link to set their password: john.doe@example.com (Researcher)"
      liftIO $ stringField "confirmation_token" structuredContent `shouldSatisfy` isJust
      -- AND: Compare state in DB with expectation
      assertAbsenceOfUserByEmailInDB requestContext "john.doe@example.com"
  it "HTTP 200 OK if the confirmation token is given (the users invited)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expUser user = user {firstName = "John", lastName = "Doe", active = True, role = toRoleSimple researcherRole} :: User
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "invite_users" (object ["confirmation_token" .= token, "users" .= [reqUser]])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "invite_users" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserByEmailInDB requestContext "john.doe@example.com" expUser
  it "HTTP 200 OK if another role than the default one is given and the user has no mcp.sensitive (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "invite_users" (object ["users" .= [object ["firstName" .= ("John" :: String), "lastName" .= ("Doe" :: String), "email" .= ("john.doe@example.com" :: String), "roleUuid" .= adminRole.uuid]]])
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
