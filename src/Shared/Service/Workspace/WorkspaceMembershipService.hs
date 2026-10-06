module Shared.Service.Workspace.WorkspaceMembershipService where

import Control.Monad (unless, void)
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.Workspace.Workspace
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.User.GroupMembership.UserGroupMembershipService
import Shared.Service.Workspace.WorkspaceMapper

addUserToSingleWorkspace :: WizardRequestContextC s m => U.UUID -> U.UUID -> UTCTime -> m ()
addUserToSingleWorkspace tenantUuid userUuid now = do
  tenant <- findTenantByUuid tenantUuid
  unless tenant.multiWorkspace $ do
    workspaces <- findWorkspacesByTenantUuid tenantUuid
    case workspaces of
      [workspace] -> addWorkspaceMembership workspace userUuid Nothing now
      [] -> throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
      _ -> throwError $ UserError _ERROR_SERVICE_WORKSPACE__MULTIPLE_WORKSPACES

addWorkspaceMembership :: WizardRequestContextC s m => Workspace -> U.UUID -> Maybe U.UUID -> UTCTime -> m ()
addWorkspaceMembership workspace userUuid mRoleUuid now = do
  roleUuid <- resolveMemberRoleUuid workspace mRoleUuid
  void $ insertWorkspaceMembership (toMembership workspace.uuid userUuid roleUuid workspace.tenantUuid now)

resolveMemberRoleUuid :: WizardRequestContextC s m => Workspace -> Maybe U.UUID -> m U.UUID
resolveMemberRoleUuid workspace Nothing = maybe (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "role")) return workspace.defaultRoleUuid
resolveMemberRoleUuid workspace (Just roleUuid) = do
  role <- findRoleByUuidAndTenant roleUuid workspace.tenantUuid
  unless (role.workspaceUuid == Just workspace.uuid) (throwError . UserError $ _ERROR_VALIDATION__USER_ROLE_NOT_IN_WORKSPACE)
  return roleUuid

removeWorkspaceMembership :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeWorkspaceMembership workspaceUuid userUuid = do
  userGroupUuids <- findUserGroupUuidsInWorkspaceByUserUuid workspaceUuid userUuid
  projects <- findProjectsSimpleWithPermByWorkspaceUuidAndUserUuid workspaceUuid userUuid
  deleteUserGroupMembershipsInWorkspaceByUserUuid workspaceUuid userUuid
  deleteProjectPermsInWorkspaceByUserUuid workspaceUuid userUuid
  void $ deleteWorkspaceMembership workspaceUuid userUuid
  traverse_ (\userGroupUuid -> removeUserGroupMembersFromOnlineUsers userGroupUuid [userUuid]) userGroupUuids
  traverse_ (\project -> updatePermsForOnlineUsers project.uuid workspaceUuid project.visibility project.sharing (filter (\perm -> perm.memberUuid /= userUuid) project.permissions)) projects
  traverse_ (deleteProjectCacheByProjectUuid . (.uuid)) projects
