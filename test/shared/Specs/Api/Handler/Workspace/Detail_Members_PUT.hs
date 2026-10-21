module Specs.Api.Handler.Workspace.Detail_Members_PUT (
  detail_members_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (execute_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeJM ()
import Shared.Database.DAO.Common
import Shared.Database.DAO.User.UserDAO
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
test_204 requestContext = do
  test_204_added requestContext
  test_204_role requestContext
  test_204_self requestContext
  test_204_inactive requestContext
  test_204_mail_failure requestContext

test_204_added requestContext =
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
    -- AND: Compare state in DB with expectation
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2
    assertMailsInDB requestContext [workspaceMemberMail "workspaceMemberAdded" userNikola secondWorkspace (roleMailParameters "role" secondWorkspaceUserRole)]
    -- AND: Adding again is idempotent
    response2 <- request reqMethod reqUrl reqHeaders reqBody
    response2 `shouldRespondWith` responseMatcher
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2
    assertMailsInDB requestContext [workspaceMemberMail "workspaceMemberAdded" userNikola secondWorkspace (roleMailParameters "role" secondWorkspaceUserRole)]

test_204_role requestContext =
  it "HTTP 204 NO CONTENT if a workspace role is assigned and changed" $ do
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
    -- AND: Compare state in DB with expectation
    membership <- getOneFromDB (findWorkspaceMembership secondWorkspace.uuid userNikola.uuid) requestContext
    liftIO $ membership.roleUuid `shouldBe` secondWorkspaceAdminRole.uuid
    -- AND: The role of an existing member can be changed
    response2 <- request reqMethod reqUrl reqHeaders reqBody
    response2 `shouldRespondWith` responseMatcher
    membership2 <- getOneFromDB (findWorkspaceMembership secondWorkspace.uuid userNikola.uuid) requestContext
    liftIO $ membership2.roleUuid `shouldBe` secondWorkspaceUserRole.uuid
    assertMailsInDB
      requestContext
      [ workspaceMemberMail "workspaceMemberAdded" userNikola secondWorkspace (roleMailParameters "role" secondWorkspaceAdminRole)
      , workspaceMemberMail "workspaceMemberRoleChanged" userNikola secondWorkspace (roleMailParameters "role" secondWorkspaceAdminRole ++ roleMailParameters "newRole" secondWorkspaceUserRole)
      ]

test_204_self requestContext =
  it "HTTP 204 NO CONTENT if the current user adds themselves (no mail)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspaceWithoutMember requestContext
    -- WHEN: Call API
    response <- request reqMethod (BS.pack $ "/api/workspaces/" ++ U.toString secondWorkspace.uuid ++ "/members/" ++ U.toString userAlbert.uuid) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    assertCountInDB (findWorkspaceMembershipsByUserUuid userAlbert.uuid) requestContext 2
    assertMailsInDB requestContext []

test_204_inactive requestContext =
  it "HTTP 204 NO CONTENT if the user is inactive (no mail)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    runInContextIO (updateUserByUuid (userNikola {active = False} :: User)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2
    assertMailsInDB requestContext []

test_204_mail_failure requestContext =
  it "HTTP 204 NO CONTENT if the mail can not be stored (the member is still added)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    runInContextIO (runDB (`execute_` failingMailTriggerSql)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    runInContextIO (runDB (`execute_` dropFailingMailTriggerSql)) requestContext
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Compare state in DB with expectation
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 2
    assertMailsInDB requestContext []

failingMailTriggerSql =
  "CREATE OR REPLACE FUNCTION fail_mail() RETURNS trigger AS $$ BEGIN RAISE EXCEPTION 'mail failure'; END $$ LANGUAGE plpgsql; \
  \DROP TRIGGER IF EXISTS fail_mail ON persistent_command; CREATE TRIGGER fail_mail BEFORE INSERT ON persistent_command FOR EACH ROW EXECUTE FUNCTION fail_mail();"

dropFailingMailTriggerSql =
  "DROP TRIGGER IF EXISTS fail_mail ON persistent_command; \
  \DROP FUNCTION IF EXISTS fail_mail();"

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
    -- AND: Compare state in DB with expectation
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
    -- AND: Compare state in DB with expectation
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
