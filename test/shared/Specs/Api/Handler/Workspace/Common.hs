module Specs.Api.Handler.Workspace.Common where

import Control.Monad (void)
import Data.Aeson (encode)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.User.RoleMapper (toRoleSimple)

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
enableMultiWorkspace requestContext = void $ runInContextIO (updateTenantByUuid (defaultTenant {multiWorkspace = True})) requestContext

demoteToResearcher requestContext user = void $ runInContextIO (updateUserByUuid (user {role = toRoleSimple researcherRole} :: User)) requestContext

insertSecondWorkspace requestContext = do
  enableMultiWorkspace requestContext
  runInContextIO (insertWorkspace (secondWorkspace {defaultRoleUuid = Nothing} :: Workspace)) requestContext
  runInContextIO (insertRole secondWorkspaceAdminRole) requestContext
  runInContextIO (insertRole secondWorkspaceUserRole) requestContext
  runInContextIO (updateWorkspaceByUuid secondWorkspace) requestContext
  runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userSystem)) requestContext
  void $ runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userAlbert)) requestContext

insertSecondWorkspaceWithoutCaller requestContext = do
  insertSecondWorkspaceWithoutMember requestContext
  demoteToResearcher requestContext userAlbert
  void $ runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext

insertSecondWorkspaceWithoutMember requestContext = do
  enableMultiWorkspace requestContext
  runInContextIO (insertWorkspace (secondWorkspace {defaultRoleUuid = Nothing} :: Workspace)) requestContext
  runInContextIO (insertRole secondWorkspaceAdminRole) requestContext
  runInContextIO (insertRole secondWorkspaceUserRole) requestContext
  runInContextIO (updateWorkspaceByUuid secondWorkspace) requestContext
  void $ runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userSystem)) requestContext

-- --------------------------------
-- TESTS
-- --------------------------------
createNotMemberTest requestContext prepare reqMethod reqUrl reqBody missingPerm =
  it "HTTP 403 FORBIDDEN (not a member of w)" $ do
    -- GIVEN: Run migrations
    void prepare
    insertSecondWorkspaceWithoutCaller requestContext
    -- AND: Prepare expectation
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ missingPerm)
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = resCtHeader : resCorsHeaders, matchStatus = 403, matchBody = bodyEquals (encode expDto)}
    response `shouldRespondWith` responseMatcher

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfWorkspaceInDB requestContext workspace = do
  workspaceFromDb <- getOneFromDB (findWorkspaceByUuid workspace.uuid) requestContext
  compareWorkspaceDtos workspaceFromDb workspace

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareWorkspaceDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldBe` expDto.uuid
  liftIO $ resDto.tenantUuid `shouldBe` expDto.tenantUuid
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.logo `shouldBe` expDto.logo
  liftIO $ resDto.primaryColor `shouldBe` expDto.primaryColor
