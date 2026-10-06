module Shared.Service.Workspace.WorkspaceAuditLog where

import qualified Data.UUID as U

import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.WorkspaceAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMembership
import Shared.Service.AuditLog.AuditLogMapper (toUserRef)
import Shared.Service.AuditLog.AuditLogService

al_createWorkspace :: WizardRequestContextC s m => Workspace -> m ()
al_createWorkspace workspace =
  logAuditLog workspacesCreate (workspaceEntity workspace) AuditLogWorkspaceData {description = workspace.description, primaryColor = workspace.primaryColor}

al_updateWorkspace :: WizardRequestContextC s m => Workspace -> Workspace -> m ()
al_updateWorkspace workspace updatedWorkspace =
  logAuditLogChanges
    workspacesUpdate
    (workspaceEntity updatedWorkspace)
    AuditLogWorkspaceChanges
      { name = change workspace.name updatedWorkspace.name
      , description = change workspace.description updatedWorkspace.description
      , primaryColor = change workspace.primaryColor updatedWorkspace.primaryColor
      }

al_deleteWorkspace :: WizardRequestContextC s m => Workspace -> AuditLogWorkspaceDeleteData -> m ()
al_deleteWorkspace workspace = logAuditLog workspacesDelete (workspaceEntity workspace)

al_saveWorkspaceMembership :: WizardRequestContextC s m => Workspace -> Maybe WorkspaceMembership -> WorkspaceMembership -> m ()
al_saveWorkspaceMembership workspace Nothing membership = al_addWorkspaceMember workspace membership
al_saveWorkspaceMembership workspace (Just oldMembership) membership
  | oldMembership.roleUuid /= membership.roleUuid = al_changeWorkspaceMemberRole workspace oldMembership membership
  | otherwise = return ()

al_addWorkspaceMember :: WizardRequestContextC s m => Workspace -> WorkspaceMembership -> m ()
al_addWorkspaceMember workspace membership = whenAuditLogEnabled $ do
  member <- findMemberRef membership.userUuid
  role <- findRoleRef membership.roleUuid
  logAuditLog workspacesMemberAdd (workspaceEntity workspace) AuditLogWorkspaceMemberData {member = member, role = role}

al_removeWorkspaceMember :: WizardRequestContextC s m => Workspace -> WorkspaceMembership -> m ()
al_removeWorkspaceMember workspace membership = whenAuditLogEnabled $ do
  member <- findMemberRef membership.userUuid
  role <- findRoleRef membership.roleUuid
  logAuditLog workspacesMemberRemove (workspaceEntity workspace) AuditLogWorkspaceMemberData {member = member, role = role}

al_changeWorkspaceMemberRole :: WizardRequestContextC s m => Workspace -> WorkspaceMembership -> WorkspaceMembership -> m ()
al_changeWorkspaceMemberRole workspace oldMembership membership = whenAuditLogEnabled $ do
  member <- findMemberRef membership.userUuid
  oldRole <- findRoleRef oldMembership.roleUuid
  newRole <- findRoleRef membership.roleUuid
  logAuditLog workspacesMemberRoleChange (workspaceEntity workspace) AuditLogWorkspaceMemberRoleChangeData {member = member, oldRole = oldRole, newRole = newRole}

workspaceEntity :: Workspace -> AuditLogEntity
workspaceEntity workspace = entityInWorkspace workspace.uuid workspace.uuid workspace.name

findMemberRef :: WizardRequestContextC s m => U.UUID -> m AuditLogUserRef
findMemberRef userUuid = toUserRef <$> findUserByUuid userUuid

findRoleRef :: WizardRequestContextC s m => U.UUID -> m AuditLogRef
findRoleRef roleUuid = do
  role <- findRoleByUuid roleUuid
  return AuditLogRef {uuid = role.uuid, name = role.name}
