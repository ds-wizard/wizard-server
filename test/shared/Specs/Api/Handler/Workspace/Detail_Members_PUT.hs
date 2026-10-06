module Specs.Api.Handler.Workspace.Detail_Members_PUT (
  detail_members_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeJM ()
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/workspaces/{uuid}/members/{userUuid}
-- ------------------------------------------------------------------------
detail_members_PUT :: RequestContext -> SpecWith ((), Application)
detail_members_PUT requestContext =
  describe "PUT /api/workspaces/{uuid}/members/{userUuid}" $ do
    test_204 requestContext
    test_204_role requestContext
    test_400 requestContext
    test_400_single_workspace requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_other_workspace requestContext
    test_404 requestContext
    test_404_user requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/workspaces/3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9/members/30d48cf4-8c8a-496f-bafe-585bd238f798"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = encode (WorkspaceMemberChangeDTO {roleUuid = Nothing})

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

test_204_role requestContext =
  it "HTTP 204 NO CONTENT (with a workspace role)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders (encode (WorkspaceMemberChangeDTO {roleUuid = Just secondWorkspaceAdminRole.uuid}))
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    membership <- getOneFromDB (findWorkspaceMembership secondWorkspace.uuid userNikola.uuid) requestContext
    liftIO $ membership.roleUuid `shouldBe` secondWorkspaceAdminRole.uuid
    -- AND: The role of an existing member can be changed
    response2 <- request reqMethod reqUrl reqHeaders reqBody
    response2 `shouldRespondWith` responseMatcher
    membership2 <- getOneFromDB (findWorkspaceMembership secondWorkspace.uuid userNikola.uuid) requestContext
    liftIO $ membership2.roleUuid `shouldBe` secondWorkspaceUserRole.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST when the role belongs to another workspace" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_VALIDATION__USER_ROLE_NOT_IN_WORKSPACE)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders (encode (WorkspaceMemberChangeDTO {roleUuid = Just defaultWorkspaceAdminRole.uuid}))
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST when a workspace role is assigned in a single-workspace tenant" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE)
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members/30d48cf4-8c8a-496f-bafe-585bd238f798" reqHeaders (encode (WorkspaceMemberChangeDTO {roleUuid = Just defaultWorkspaceAdminRole.uuid}))
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    membership <- getOneFromDB (findWorkspaceMembership defaultWorkspace.uuid userNikola.uuid) requestContext
    liftIO $ membership.roleUuid `shouldBe` defaultWorkspaceUserRole.uuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "members.manage"

test_403_other_workspace requestContext =
  it "HTTP 403 FORBIDDEN - the workspace Admin role of another workspace does not count" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Missing permission: members.manage"
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    demoteToResearcher requestContext userAlbert
    runInContextIO (updateWorkspaceMembershipRole defaultWorkspace.uuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 1
    -- AND: The same call on the workspace the caller administers succeeds
    response2 <- request reqMethod "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members/30d48cf4-8c8a-496f-bafe-585bd238f798" reqHeaders reqBody
    let responseMatcher2 =
          ResponseMatcher {matchHeaders = resCorsHeaders, matchStatus = 204, matchBody = bodyEquals ""}
    response2 `shouldRespondWith` responseMatcher2

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

test_404_user requestContext =
  createNotFoundTest'
    reqMethod
    "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "user_entity"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
