module Specs.Api.Handler.Workspace.Common where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Test.Hspec

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
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.User.RoleMapper (toRoleSimple)

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
  enableMultiWorkspace requestContext
  demoteToResearcher requestContext userAlbert
  runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext
  runInContextIO (insertWorkspace (secondWorkspace {defaultRoleUuid = Nothing} :: Workspace)) requestContext
  runInContextIO (insertRole secondWorkspaceAdminRole) requestContext
  runInContextIO (insertRole secondWorkspaceUserRole) requestContext
  runInContextIO (updateWorkspaceByUuid secondWorkspace) requestContext
  void $ runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userSystem)) requestContext

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
