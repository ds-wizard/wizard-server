module Specs.Api.Handler.Workspace.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common

-- ------------------------------------------------------------------------
-- POST /api/workspaces
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/workspaces" $ do
    test_201 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/workspaces"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = workspaceCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    -- AND: Run migrations
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, Workspace)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    let expDto = secondWorkspace {uuid = resBody.uuid, name = reqDto.name, description = reqDto.description, primaryColor = reqDto.primaryColor} :: Workspace
    compareWorkspaceDtos resBody expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfWorkspaceInDB requestContext expDto
    assertCountInDB findWorkspaces requestContext 2
    assertCountInDB (findWorkspaceMembershipsByUserUuid userAlbert.uuid) requestContext 2
    assertCountInDB (findWorkspaceMembershipsByUserUuid userSystem.uuid) requestContext 2
    -- AND: The workspace has its Admin and User roles
    roles <- getOneFromDB (findRolesOfWorkspace resBody.uuid) requestContext
    liftIO $ length roles `shouldBe` 2
    let mAdminRole = find (\role -> role.name == "Admin") roles
    let mUserRole = find (\role -> role.name == "User") roles
    liftIO $ fmap (\role -> (role.permissions, role.isAdmin)) mAdminRole `shouldBe` Just (workspaceRolePermissions, False)
    liftIO $ fmap (.permissions) mUserRole `shouldBe` Just []
    liftIO $ resBody.defaultRoleUuid `shouldBe` fmap (.uuid) mUserRole
    -- AND: The creator is Admin, the system user is User
    albertMembership <- getOneFromDB (findWorkspaceMembership resBody.uuid userAlbert.uuid) requestContext
    liftIO $ Just albertMembership.roleUuid `shouldBe` fmap (.uuid) mAdminRole
    systemMembership <- getOneFromDB (findWorkspaceMembership resBody.uuid userSystem.uuid) requestContext
    liftIO $ Just systemMembership.roleUuid `shouldBe` fmap (.uuid) mUserRole

findRolesOfWorkspace :: U.UUID -> RequestContextM [Role]
findRolesOfWorkspace workspaceUuid = filter (\role -> role.workspaceUuid == Just workspaceUuid) <$> findRoles

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "name"
  it "HTTP 400 BAD REQUEST when the name is blank" $ do
    -- GIVEN: Prepare request
    let reqBody = encode (workspaceCreate {name = "   "} :: WorkspaceChangeDTO)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ValidationError [] (M.singleton "name" [_ERROR_VALIDATION__FIELDS_ABSENCE])
    let expBody = encode expDto
    -- AND: Run migrations
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findWorkspaces requestContext 1
  it "HTTP 400 BAD REQUEST when the tenant is single-workspace" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findWorkspaces requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "workspaces.manage"
