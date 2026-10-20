module Specs.Api.Handler.UserGroup.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Data.Maybe (isJust, isNothing)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Api.Resource.User.Group.UserGroupJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/user-groups
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/user-groups" $ do
    test_201 requestContext
    test_201_workspace requestContext
    test_201_no_owner requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_workspace requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/user-groups"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = encode userGroupChange

expUserGroup = bioGroup {name = userGroupChange.name, description = userGroupChange.description, private = userGroupChange.private} :: UserGroup

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext =
  it "HTTP 201 CREATED (single-workspace tenant gets the only workspace, creator becomes owner)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, UserGroup)
    assertResStatus status 201
    liftIO $ compareUserGroups resBody expUserGroup `shouldBe` True
    -- AND: Find result in DB and compare with expectation state
    mMembership <- getOneFromDB (findUserGroupMembershipByUserGroupUuidAndUserUuid' resBody.uuid userAlbert.uuid) requestContext
    liftIO $ isJust mMembership `shouldBe` True

test_201_workspace requestContext =
  it "HTTP 201 CREATED (w in a multi-workspace tenant)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?" <> secondWorkspaceQuery) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, UserGroup)
    assertResStatus status 201
    liftIO $ compareUserGroups resBody (expUserGroup {workspaceUuid = secondWorkspaceUuid}) `shouldBe` True

test_201_no_owner requestContext =
  it "HTTP 201 CREATED (creator outside the workspace does not become owner)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspaceWithoutMember requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?" <> secondWorkspaceQuery) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, UserGroup)
    assertResStatus status 201
    -- AND: Find result in DB and compare with expectation state
    mMembership <- getOneFromDB (findUserGroupMembershipByUserGroupUuidAndUserUuid' resBody.uuid userAlbert.uuid) requestContext
    liftIO $ isNothing mMembership `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  create_test_400 "HTTP 400 BAD REQUEST (w in a single-workspace tenant)" requestContext (return ()) ("?" <> workspaceQuery) _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED
  create_test_400 "HTTP 400 BAD REQUEST (no w in a multi-workspace tenant)" requestContext (enableMultiWorkspace requestContext) "" _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED
  create_test_400 "HTTP 400 BAD REQUEST (tenant=true)" requestContext (return ()) "?tenant=true" _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED

create_test_400 title requestContext prepare query expError =
  it title $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    prepare
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> query) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError expError) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (no userGroups.create)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _USER_GROUPS_CREATE_ROLE_PERMISSION)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 403 (ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION _USER_GROUPS_CREATE_ROLE_PERMISSION) response

test_403_workspace requestContext =
  it "HTTP 403 FORBIDDEN (no userGroups.create in w)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _USER_GROUPS_CREATE_ROLE_PERMISSION)) requestContext
    runInContextIO (updateWorkspaceMembershipRole secondWorkspaceUuid userAlbert.uuid secondWorkspaceUserRole.uuid) requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?" <> secondWorkspaceQuery) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 403 (ForbiddenError $ _ERROR_VALIDATION__MISSING_PERMISSION _USER_GROUPS_CREATE_ROLE_PERMISSION) response
