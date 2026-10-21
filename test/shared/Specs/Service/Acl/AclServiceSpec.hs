module Specs.Service.Acl.AclServiceSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Api.Resource.User.UserDTO
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Context.Scope
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Service.Acl.AclService
import Shared.Service.User.RoleMapper (toRoleSimple)
import qualified Shared.Service.User.WizardUserMapper as U_Mapper

aclServiceSpec =
  describe "AclService" $ do
    let user = (U_Mapper.toDTO userNikola) {role = toRoleSimple researcherRole} :: UserDTO
    let workspaceRoles = M.fromList [(defaultWorkspaceUuid, toRoleSimple defaultWorkspaceAdminRole), (secondWorkspaceUuid, toRoleSimple secondWorkspaceUserRole)]
    it "the organization role alone counts on the tenant plane" $
      effectivePermissions user workspaceRoles True TenantScope `shouldBe` researcherRole.permissions
    it "the workspace role is added for its workspace" $
      (_PROJECTS_VIEW_ROLE_PERMISSION `elem` effectivePermissions user workspaceRoles True (WorkspaceScope defaultWorkspaceUuid)) `shouldBe` True
    it "the workspace role does not reach another workspace" $
      (_PROJECTS_VIEW_ROLE_PERMISSION `elem` permissionsInWorkspace user workspaceRoles True (Just secondWorkspaceUuid)) `shouldBe` False
    it "a workspace scope without a role there keeps the organization role alone" $
      effectivePermissions user workspaceRoles True (WorkspaceScope differentWorkspaceUuid) `shouldBe` researcherRole.permissions
    it "without a scope every workspace role counts" $
      (_PROJECTS_VIEW_ROLE_PERMISSION `elem` effectivePermissions user workspaceRoles True NoScope) `shouldBe` True
    it "a single-workspace tenant ignores the workspace role without a scope" $
      effectivePermissions user workspaceRoles False NoScope `shouldBe` researcherRole.permissions
    it "a single-workspace tenant ignores the workspace role in its workspace" $
      effectivePermissions user workspaceRoles False (WorkspaceScope defaultWorkspaceUuid) `shouldBe` researcherRole.permissions
    it "a single-workspace tenant ignores the workspace role on the tenant plane" $
      permissionsInWorkspace user workspaceRoles False Nothing `shouldBe` researcherRole.permissions
    it "a multi-workspace tenant keeps the tenant plane to the organization role" $
      (_PROJECTS_VIEW_ROLE_PERMISSION `elem` permissionsInWorkspace user workspaceRoles True Nothing) `shouldBe` False
    it "the organization role reaches every workspace" $
      (_PROJECTS_VIEW_ROLE_PERMISSION `elem` permissionsInWorkspace (user {role = toRoleSimple adminRole}) M.empty True (Just secondWorkspaceUuid)) `shouldBe` True
    it "projects.create alone does not reach a workspace" $
      (_PROJECTS_CREATE_ROLE_PERMISSION `elem` workspaceReachRolePermissions) `shouldBe` False
    it "userGroups.create alone does not reach a workspace" $
      (_USER_GROUPS_CREATE_ROLE_PERMISSION `elem` workspaceReachRolePermissions) `shouldBe` False
    it "every other workspace permission reaches a workspace" $
      workspaceReachRolePermissions `shouldBe` filter (`notElem` [_PROJECTS_CREATE_ROLE_PERMISSION, _USER_GROUPS_CREATE_ROLE_PERMISSION]) workspaceRolePermissions
    it "workspaces.manage cannot be assigned" $
      (_WORKSPACES_MANAGE_ROLE_PERMISSION `elem` assignableRolePermissions) `shouldBe` False
    it "library.manage cannot be assigned" $
      (_LIBRARY_MANAGE_ROLE_PERMISSION `elem` assignableRolePermissions) `shouldBe` False
