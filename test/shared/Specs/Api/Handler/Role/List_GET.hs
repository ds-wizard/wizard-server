module Specs.Api.Handler.Role.List_GET (
  list_GET,
) where

import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.User.RoleListJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RoleList (RoleList)
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import qualified Shared.Service.User.RoleMapper as Mapper
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/roles
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/roles" $ do
    test_200 requestContext
    test_200_workspace requestContext
    test_200_members_manage requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/roles"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (sort by uuid asc)"
    requestContext
    "/api/roles?sort=uuid,asc"
    ( Page
        "roles"
        (PageMetadata 20 3 1 0)
        [Mapper.toDTO adminRole 2, Mapper.toDTO dataStewardRole 1, Mapper.toDTO researcherRole 1]
    )
  create_test_200
    "HTTP 200 OK (pagination)"
    requestContext
    "/api/roles?sort=uuid,asc&page=1&size=1"
    (Page "roles" (PageMetadata 1 3 3 1) [Mapper.toDTO dataStewardRole 1])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/api/roles?sort=uuid,asc&q=Researcher"
    (Page "roles" (PageMetadata 20 1 1 0) [Mapper.toDTO researcherRole 1])

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_workspace requestContext = do
  create_test_200_multi
    "HTTP 200 OK (tenant=true lists the organization roles)"
    requestContext
    "/api/roles?tenant=true&sort=uuid,asc"
    (Page "roles" (PageMetadata 20 3 1 0) [Mapper.toDTO adminRole 2, Mapper.toDTO dataStewardRole 1, Mapper.toDTO researcherRole 1])
  create_test_200_multi
    "HTTP 200 OK (w lists the roles of the workspace)"
    requestContext
    (BS.pack $ "/api/roles?sort=uuid,asc&w=" ++ U.toString defaultWorkspaceUuid)
    (Page "roles" (PageMetadata 20 2 1 0) [Mapper.toDTO defaultWorkspaceAdminRole 1, Mapper.toDTO defaultWorkspaceUserRole 2])

create_test_200_multi title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_members_manage requestContext = do
  it "HTTP 200 OK (members.manage on the workspace role reads that plane only)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            Page
              "roles"
              (PageMetadata 20 3 1 0)
              [Mapper.toDTO defaultWorkspaceAdminRole 0, Mapper.toDTO defaultWorkspaceUserRole 2, Mapper.toDTO membersManageRole 1]
      let expBody = encode expDto
      -- AND: Run migrations
      prepareMembersManager requestContext
      -- WHEN: Call API
      response <- request reqMethod (BS.pack $ "/api/roles?sort=uuid,asc&w=" ++ U.toString defaultWorkspaceUuid) reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 403 FORBIDDEN (members.manage on the workspace role does not open the organization plane)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission (need any): " ++ show [_ROLES_MANAGE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION, _MEMBERS_MANAGE_ROLE_PERMISSION])
      let expBody = encode expDto
      -- AND: Run migrations
      prepareMembersManager requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/roles?tenant=true" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK (members.manage does not reach a workspace the caller is not a member of)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = Page "roles" (PageMetadata 20 0 0 0) ([] :: [RoleList])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO (insertRole membersManageRole) requestContext
      insertSecondWorkspaceWithoutCaller requestContext
      runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid membersManageRole.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod (BS.pack $ "/api/roles?w=" ++ U.toString secondWorkspaceUuid) reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

membersManageRole :: Role
membersManageRole =
  defaultWorkspaceAdminRole
    { uuid = u' "a0000000-0000-0000-0000-000000000023"
    , name = "Member Manager"
    , permissions = [_MEMBERS_MANAGE_ROLE_PERMISSION]
    }
    :: Role

prepareMembersManager requestContext = do
  runInContextIO U.runMigration requestContext
  runInContextIO (insertRole membersManageRole) requestContext
  demoteToResearcher requestContext userAlbert
  runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid membersManageRole.uuid) requestContext
  enableMultiWorkspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400 "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" requestContext False (BS.pack $ "/api/roles?w=" ++ U.toString defaultWorkspaceUuid) _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED
  create_test_400 "HTTP 400 BAD REQUEST (no plane in a multi-workspace tenant)" requestContext True "/api/roles" _ERROR_SERVICE_WORKSPACE__SCOPE_REQUIRED
  create_test_400 "HTTP 400 BAD REQUEST (w and tenant)" requestContext True (BS.pack $ "/api/roles?tenant=true&w=" ++ U.toString defaultWorkspaceUuid) _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT

create_test_400 title requestContext multiWorkspace reqUrl expError =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError expError)
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      when multiWorkspace (enableMultiWorkspace requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionsAnyTest requestContext reqMethod reqUrl [] reqBody ["roles.manage", "users.manage", "members.manage"]
