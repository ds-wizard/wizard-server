module Specs.Api.Handler.Workspace.Member_DELETE (
  member_DELETE,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectPermDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/workspaces/{uuid}/members/{userUuid}
-- ------------------------------------------------------------------------
member_DELETE :: RequestContext -> SpecWith ((), Application)
member_DELETE requestContext =
  describe "DELETE /api/workspaces/{uuid}/members/{userUuid}" $ do
    test_204 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members/30d48cf4-8c8a-496f-bafe-585bd238f798"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT - group memberships and project permissions of the member are gone" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO (insertPackage amsterdamKmPackage) requestContext
    runInContextIO (insertProject project14) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB (findWorkspaceMembershipsByUserUuid userNikola.uuid) requestContext 0
    assertCountInDB (findUserGroupMembershipsByUserUuid userNikola.uuid) requestContext 0
    assertCountInDB (findProjectPermsFiltered [("project_uuid", "8355fe3c-47b9-4078-b5b6-08aa0188e85f")]) requestContext 0
    assertCountInDB (findUserGroupMembershipsByUserUuid userAlbert.uuid) requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST - the system user can not be removed" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_SERVICE_WORKSPACE__SYSTEM_USER
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members/00000000-0000-0000-0000-000000000000" reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB (findWorkspaceMembershipsByUserUuid userSystem.uuid) requestContext 1

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
