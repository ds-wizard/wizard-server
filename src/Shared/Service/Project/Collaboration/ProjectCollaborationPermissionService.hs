module Shared.Service.Project.Collaboration.ProjectCollaborationPermissionService where

import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.Websocket.WebsocketConnectionDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Project
import Shared.Model.Tenant.Tenant
import Shared.Model.User.OnlineUserInfo
import Shared.Model.User.RolePermission
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Service.Acl.AclService
import Shared.Service.Project.Collaboration.ProjectCollaborationAcl
import Shared.Service.Websocket.WebsocketRegistryService
import Shared.Service.Websocket.WebsocketService
import Shared.Service.Workspace.WorkspaceScopeService

type WorkspaceAccess = M.Map U.UUID ([String], Bool)

updateProjectPermissions :: WizardRequestContextC s m => U.UUID -> m Bool
updateProjectPermissions projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  connections <- findLocalConnections (tenantUuid, ProjectWebsocketSubscription, projectUuid)
  if null connections
    then return False
    else findProjectByUuid projectUuid >>= updateProjectConnections connections

updateWorkspacePermissions :: WizardRequestContextC s m => U.UUID -> m [U.UUID]
updateWorkspacePermissions workspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  projectUuids <- findLocalEntityUuids tenantUuid ProjectWebsocketSubscription
  projects <- traverse findProjectByUuid' projectUuids
  let workspaceProjects = [project | Just project <- projects, project.workspaceUuid == workspaceUuid]
  removed <- traverse (\project -> findLocalConnections (tenantUuid, ProjectWebsocketSubscription, project.uuid) >>= (`updateProjectConnections` project)) workspaceProjects
  return [project.uuid | (project, True) <- zip workspaceProjects removed]

updateProjectConnections :: WizardRequestContextC s m => [LocalWebsocketConnection] -> Project -> m Bool
updateProjectConnections connections project = do
  tenantUuid <- asks (.tenantUuid')
  tenant <- findTenantByUuid tenantUuid
  let users = M.fromList [(uuid, connection.tenantRolePermissions) | connection@LocalWebsocketConnection {onlineUser = LoggedOnlineUserInfo {uuid = uuid}} <- connections]
  access <- M.traverseWithKey (resolveWorkspaceAccess tenant.multiWorkspace project.workspaceUuid) users
  let key = (tenantUuid, ProjectWebsocketSubscription, project.uuid)
  or <$> traverse (updatePermission key project access) connections

resolveWorkspaceAccess :: WizardRequestContextC s m => Bool -> U.UUID -> U.UUID -> [String] -> m ([String], Bool)
resolveWorkspaceAccess multiWorkspace workspaceUuid userUuid tenantRolePermissions = do
  workspaceRoles <- findWorkspaceRolesByUserUuid userUuid
  let permissions = tenantRolePermissions ++ (if multiWorkspace then workspacePermissions workspaceRoles workspaceUuid else [])
  reachable <-
    if M.member workspaceUuid workspaceRoles || any (`elem` tenantRolePermissions) workspaceReachRolePermissions
      then return True
      else isWorkspaceMember workspaceUuid userUuid
  return (permissions, reachable)

updatePermission :: WizardRequestContextC s m => WebsocketEntityKey -> Project -> WorkspaceAccess -> LocalWebsocketConnection -> m Bool
updatePermission key project access connection = do
  let permission = computePermission project access connection
  unless (permission == connection.websocketPerm) $ do
    updateWebsocketConnectionPermByUuid connection.connectionUuid permission
    adjustLocalConnection key connection.connectionUuid (\c -> c {websocketPerm = permission})
  catchError (checkViewPermission permission >> return False) $ \error -> do
    disconnectUserWithError key connection error
    return True

computePermission :: Project -> WorkspaceAccess -> LocalWebsocketConnection -> WebsocketPerm
computePermission project access connection =
  case connection.onlineUser of
    LoggedOnlineUserInfo {uuid = uuid} ->
      let (permissions, reachable) = M.findWithDefault ([], False) uuid access
       in getPermission project.visibility project.sharing project.permissions (Just uuid) permissions connection.userGroupUuids reachable
    AnonymousOnlineUserInfo {} ->
      getPermission project.visibility project.sharing project.permissions Nothing [] [] True

removeUserGroup :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroup userGroupUuid userUuids = do
  tenantUuid <- asks (.tenantUuid')
  nodeUuid <- asks (.nodeUuid')
  rows <- removeUserGroupFromWebsocketConnections nodeUuid tenantUuid userGroupUuid userUuids
  traverse_ (\(connectionUuid, projectUuid) -> adjustLocalConnection (tenantUuid, ProjectWebsocketSubscription, projectUuid) connectionUuid strip) rows
  where
    strip connection =
      let userGroupUuids = filter (/= userGroupUuid) connection.userGroupUuids
       in connection {userGroupUuids = userGroupUuids, onlineUser = withGroups userGroupUuids connection.onlineUser}
    withGroups userGroupUuids user@LoggedOnlineUserInfo {} = user {groupUuids = userGroupUuids}
    withGroups _ user = user
