module Shared.Service.Workspace.WorkspaceService where

import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Constant.User
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Model.User.Role
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMember
import Shared.S3.Workspace.WorkspaceS3
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Workspace.WorkspaceMapper
import Shared.Service.Workspace.WorkspaceMembershipService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.String (trim)
import Shared.Util.Uuid

getWorkspacesPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page Workspace)
getWorkspacesPage mQuery pageable sort = do
  _ <- getCurrentUser
  managesWorkspaces <- hasPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
  accessCondition <- if managesWorkspaces then return "" else workspaceOnlyCondition Nothing "uuid"
  findWorkspacesPage accessCondition mQuery pageable sort

createWorkspace :: WizardRequestContextC s m => WorkspaceChangeDTO -> m Workspace
createWorkspace reqDto =
  runInTransaction $ do
    checkPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
    validateChangeDto reqDto
    tenantUuid <- asks (.tenantUuid')
    lockTenantByUuid tenantUuid
    tenant <- findTenantByUuid tenantUuid
    unless tenant.multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
    currentUser <- getCurrentUser
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    (workspace, adminRole) <- createWorkspaceWithRoles (fromChangeDTO reqDto uuid tenantUuid now)
    addWorkspaceMembership workspace currentUser.uuid (Just adminRole.uuid) now
    return workspace

createDefaultWorkspace :: WizardRequestContextC s m => U.UUID -> String -> UTCTime -> m Workspace
createDefaultWorkspace tenantUuid name now = do
  uuid <- liftIO generateUuid
  fst <$> createWorkspaceWithRoles (fromChangeDTO (WorkspaceChangeDTO {name = name, description = Nothing, primaryColor = Nothing}) uuid tenantUuid now)

getWorkspaceByUuid :: WizardRequestContextC s m => U.UUID -> m Workspace
getWorkspaceByUuid uuid = do
  managesWorkspaces <- hasPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
  unless managesWorkspaces (checkWorkspaceReachable uuid)
  mWorkspace <- findWorkspaceByUuid' uuid
  maybe (throwError $ NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")) return mWorkspace

modifyWorkspace :: WizardRequestContextC s m => U.UUID -> WorkspaceChangeDTO -> m Workspace
modifyWorkspace uuid reqDto =
  runInTransaction $ do
    checkPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
    validateChangeDto reqDto
    workspace <- findWorkspaceByUuid uuid
    now <- liftIO getCurrentTime
    let updatedWorkspace = fromChange workspace reqDto now
    updateWorkspaceByUuid updatedWorkspace
    return updatedWorkspace

deleteWorkspace :: WizardRequestContextC s m => U.UUID -> m ()
deleteWorkspace uuid =
  runInTransaction $ do
    checkPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
    workspace <- findWorkspaceByUuid uuid
    workspaceUuids <- findWorkspaceUuidsForUpdate
    when (length workspaceUuids < 2) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__LAST_WORKSPACE)
    projectUuids <- findProjectUuidsByWorkspaceUuid uuid
    editorUuids <- findKnowledgeModelEditorUuidsByWorkspaceUuid uuid
    void $ deleteWorkspaceByUuid uuid
    traverse_ (removeWorkspaceLogo uuid) workspace.logo
    traverse_ logOutOnlineUsersWhenProjectDramaticallyChanged projectUuids
    traverse_ logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged editorUuids

getWorkspaceMembersPage :: WizardRequestContextC s m => U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page WorkspaceMember)
getWorkspaceMembersPage uuid mQuery pageable sort = do
  checkWorkspaceReachable uuid
  findWorkspaceMembersPage uuid mQuery pageable sort

addWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> WorkspaceMemberChangeDTO -> m ()
addWorkspaceMember uuid userUuid reqDto =
  runInTransaction $ do
    checkPermissionInWorkspace _MEMBERS_MANAGE_ROLE_PERMISSION (Just uuid)
    when (userUuid == systemUserUuid) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SYSTEM_USER)
    workspace <- findWorkspaceByUuid uuid
    _ <- findUserByUuid userUuid
    checkWorkspacePlaneAvailable
    now <- liftIO getCurrentTime
    addWorkspaceMembership workspace userUuid reqDto.roleUuid now

removeWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeWorkspaceMember uuid userUuid =
  runInTransaction $ do
    checkPermissionInWorkspace _MEMBERS_MANAGE_ROLE_PERMISSION (Just uuid)
    when (userUuid == systemUserUuid) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SYSTEM_USER)
    _ <- findWorkspaceByUuid uuid
    multiWorkspace <- asks (.tenantMultiWorkspace')
    unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
    removeWorkspaceMembership uuid userUuid

createWorkspaceWithRoles :: WizardRequestContextC s m => Workspace -> m (Workspace, Role)
createWorkspaceWithRoles workspace = do
  insertWorkspace workspace
  adminRoleUuid <- liftIO generateUuid
  userRoleUuid <- liftIO generateUuid
  let adminRole = toWorkspaceRole adminRoleUuid "Admin" workspaceRolePermissions workspace
  let userRole = toWorkspaceRole userRoleUuid "User" [] workspace
  insertRole adminRole
  insertRole userRole
  let workspaceWithDefault = workspace {defaultRoleUuid = Just userRole.uuid}
  updateWorkspaceByUuid workspaceWithDefault
  addWorkspaceMembership workspaceWithDefault systemUserUuid Nothing workspace.createdAt
  return (workspaceWithDefault, adminRole)

validateChangeDto :: WizardRequestContextC s m => WorkspaceChangeDTO -> m ()
validateChangeDto reqDto =
  when (null (trim reqDto.name)) (throwError $ ValidationError [] (M.singleton "name" [_ERROR_VALIDATION__FIELDS_ABSENCE]))
