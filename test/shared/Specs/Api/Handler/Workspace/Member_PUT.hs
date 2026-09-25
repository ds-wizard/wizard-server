module Specs.Api.Handler.Workspace.Member_PUT (
  member_PUT,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/workspaces/{uuid}/members/{userUuid}
-- ------------------------------------------------------------------------
member_PUT :: RequestContext -> SpecWith ((), Application)
member_PUT requestContext =
  describe "PUT /api/workspaces/{uuid}/members/{userUuid}" $ do
    test_204 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/workspaces/3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9/members/30d48cf4-8c8a-496f-bafe-585bd238f798"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2
    -- AND: Adding again is idempotent
    response2 <- request reqMethod reqUrl reqHeaders reqBody
    response2 `shouldRespondWith` responseMatcher
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "UsersManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/workspaces/deab6c38-aeac-4b17-a501-4365a0a70176/members/30d48cf4-8c8a-496f-bafe-585bd238f798"
    reqHeaders
    reqBody
    "workspace"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
