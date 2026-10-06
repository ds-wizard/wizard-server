module Specs.Api.Handler.Role.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Api.Resource.User.RoleChangeJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.User.RoleDAO (findRoleByUuid, findRoles)
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RoleList
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Role.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/roles
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/roles" $ do
    test_201 requestContext
    test_201_workspace requestContext
    test_400 requestContext
    test_400_workspace requestContext
    test_400_single_workspace requestContext
    test_403_workspace requestContext
    test_400_invalid_permission requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/roles"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: RoleChangeDTO
reqDto =
  RoleChangeDTO
    { name = "Reviewer"
    , permissions = [_PROJECTS_VIEW_ROLE_PERMISSION]
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          RoleList
            { uuid = U.nil
            , name = reqDto.name
            , permissions = reqDto.permissions
            , usersCount = 0
            , isAdmin = False
            , workspaceUuid = Nothing
            }
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, RoleList)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareRoleDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findRoles requestContext 6

test_201_workspace requestContext =
  it "HTTP 201 CREATED (workspace role)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?w=" <> BS.pack (U.toString defaultWorkspaceUuid)) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, RoleList)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    liftIO $ resDto.workspaceUuid `shouldBe` Just defaultWorkspaceUuid
    roleFromDb <- getOneFromDB (findRoleByUuid resDto.uuid) requestContext
    liftIO $ roleFromDb.workspaceUuid `shouldBe` Just defaultWorkspaceUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod reqUrl "name"

test_400_workspace requestContext = do
  createWorkspaceValidationTest
    requestContext
    "an organization-only permission is granted on a workspace role"
    (reqUrl <> "?w=" <> BS.pack (U.toString defaultWorkspaceUuid))
    (reqDto {permissions = [_USERS_MANAGE_ROLE_PERMISSION]} :: RoleChangeDTO)
    (UserError $ _ERROR_VALIDATION__USER_ROLE_ORGANIZATION_ONLY_PERMISSION _USERS_MANAGE_ROLE_PERMISSION)
  createWorkspaceValidationTest
    requestContext
    "no plane is given in a multi-workspace tenant"
    reqUrl
    reqDto
    (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED)

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST when a workspace role is created in a single-workspace tenant" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED)
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?w=" <> BS.pack (U.toString defaultWorkspaceUuid)) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findRoles requestContext 5

createWorkspaceValidationTest requestContext title url dto expDto =
  it ("HTTP 400 BAD REQUEST when " ++ title) $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod url reqHeaders (encode dto)
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN - roles.manage on a workspace role does not reach the organization plane" $ do
    -- GIVEN: Prepare request
    let user = userWithoutPerm requestContext.serverConfig _ROLES_MANAGE_ROLE_PERMISSION
    runInContextIO (updateUserByUuid user) requestContext
    runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
    enableMultiWorkspace requestContext
    -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ _ROLES_MANAGE_ROLE_PERMISSION))
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?tenant=true") reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: The workspace plane is allowed
    response2 <- request reqMethod (reqUrl <> "?w=" <> BS.pack (U.toString defaultWorkspaceUuid)) reqHeaders reqBody
    let (status, _, _) = destructResponse response2 :: (Int, ResponseHeaders, RoleList)
    assertResStatus status 201

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_permission requestContext = do
  createInvalidPermissionTest requestContext "an internal permission is used" _DEV_USE_ROLE_PERMISSION
  createInvalidPermissionTest requestContext "an unknown permission is used" "NonsenseRolePermission"

createInvalidPermissionTest requestContext title permission =
  it ("HTTP 400 BAD REQUEST when " ++ title) $ do
    -- GIVEN: Prepare request with an invalid permission
    let invalidReqBody = encode (reqDto {permissions = [permission]} :: RoleChangeDTO)
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError $ _ERROR_VALIDATION__USER_ROLE_INVALID_PERMISSION permission
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders invalidReqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "roles.manage"
