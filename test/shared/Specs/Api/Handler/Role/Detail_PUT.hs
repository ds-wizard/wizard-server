module Specs.Api.Handler.Role.Detail_PUT (
  detail_PUT,
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
import Specs.Api.Handler.Workspace.Common
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Role.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /api/roles/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: RequestContext -> SpecWith ((), Application)
detail_PUT requestContext =
  describe "PUT /api/roles/{uuid}" $ do
    test_200 requestContext
    test_400_invalid requestContext
    test_400_admin requestContext
    test_400_workspace requestContext
    test_400_single_workspace requestContext
    test_200_workspace_manager requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/api/roles/a0000000-0000-0000-0000-000000000002"

reqUrlAdmin = "/api/roles/a0000000-0000-0000-0000-000000000001"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: RoleChangeDTO
reqDto =
  RoleChangeDTO
    { name = "Data Steward (edited)"
    , permissions = [_KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          RoleList
            { uuid = dataStewardRole.uuid
            , name = reqDto.name
            , permissions = reqDto.permissions
            , usersCount = 1
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid requestContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_admin requestContext =
  it "HTTP 400 BAD REQUEST when the admin role is changed" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__USER_ROLE_ADMIN_CANNOT_BE_CHANGED
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlAdmin reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_400_workspace requestContext =
  it "HTTP 400 BAD REQUEST when an organization-only permission is granted on a workspace role" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError $ _ERROR_VALIDATION__USER_ROLE_ORGANIZATION_ONLY_PERMISSION _USERS_MANAGE_ROLE_PERMISSION)
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (BS.pack $ "/api/roles/" ++ U.toString defaultWorkspaceAdminRole.uuid) reqHeaders (encode (reqDto {permissions = [_USERS_MANAGE_ROLE_PERMISSION]} :: RoleChangeDTO))
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_400_single_workspace requestContext =
  it "HTTP 400 BAD REQUEST when a workspace role is edited in a single-workspace tenant" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE)
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (BS.pack $ "/api/roles/" ++ U.toString defaultWorkspaceUserRole.uuid) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_workspace_manager requestContext =
  it "HTTP 200 OK (roles.manage on the workspace role edits that workspace's roles only)" $ do
    -- GIVEN: Prepare request
    runInContextIO U_Migration.runMigration requestContext
    demoteToResearcher requestContext userAlbert
    runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (BS.pack $ "/api/roles/" ++ U.toString defaultWorkspaceUserRole.uuid) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resDto) = destructResponse response :: (Int, ResponseHeaders, RoleList)
    assertResStatus status 200
    liftIO $ resDto.permissions `shouldBe` reqDto.permissions
    -- AND: The organization role stays closed
    let expBody2 = encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ _ROLES_MANAGE_ROLE_PERMISSION))
    response2 <- request reqMethod reqUrl reqHeaders reqBody
    let responseMatcher2 =
          ResponseMatcher {matchHeaders = resCtHeader : resCorsHeaders, matchStatus = 403, matchBody = bodyEquals expBody2}
    response2 `shouldRespondWith` responseMatcher2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "roles.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest
    reqMethod
    "/api/roles/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "role"
    [("tenant_uuid", "00000000-0000-0000-0000-000000000000"), ("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
