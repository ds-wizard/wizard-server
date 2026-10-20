module Specs.Service.Mcp.Tool.UserTools.DeleteUser (
  delete_user,
) where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.User.UserDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Localization.Messages.Mcp.Public
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.User.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (delete_user)
-- ------------------------------------------------------------------------
delete_user :: RequestContext -> SpecWith ((), Application)
delete_user requestContext =
  describe "POST /api/mcp (delete_user)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["user_uuid" .= userNikola.uuid]

reqBody = toolCallBody "delete_user" reqArguments

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
      liftIO $ fmap (L.isInfixOf userNikola.email) (stringField "summary" structuredContent) `shouldBe` Just True
      liftIO $ isJust (stringField "confirmation_token" structuredContent) `shouldBe` True
      assertToolOutputSchema reqHeaders "delete_user" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserInDB requestContext userNikola
  it "HTTP 200 OK if the confirmation token is given (the user deleted)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object [])
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      Just token <- confirmationTokenOf <$> request reqMethod reqUrl reqHeaders reqBody
      let reqBody = toolCallBody "delete_user" (object ["user_uuid" .= userNikola.uuid, "confirmation_token" .= token])
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      mUser <- getOneFromDB (findUserByUuid' userNikola.uuid) requestContext
      liftIO $ fmap (.uuid) mUser `shouldBe` Nothing
  it "HTTP 200 OK if the user is the last active admin (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "delete_user" (object ["user_uuid" .= userAlbert.uuid])
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
  createToolNoPermissionTest requestContext reqBody _MCP_SENSITIVE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
