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
import Shared.Model.AuditLog.Data.WorkspaceAuditLogData
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
import Shared.Service.AuditLog.AuditLogService (whenAuditLogEnabled)
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Workspace.WorkspaceAuditLog
import Shared.Service.Workspace.WorkspaceMapper
import Shared.Service.Workspace.WorkspaceMembershipService
import Shared.Service.Workspace.WorkspaceNotification
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.String (trim)
import Shared.Util.Uuid

getWorkspacesPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page Workspace)
getWorkspacesPage mQuery pageable sort = do
  _ <- getCurrentUser
  managesWorkspaces <- hasPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
  accessCondition <- if managesWorkspaces then return "" else workspaceOnlyCondition Nothing "uuid"
  findWorkspacesPage accessCondition mQuery pageable sort

getAccessibleWorkspaces :: WizardRequestContextC s m => m [Workspace]
getAccessibleWorkspaces = do
  managesWorkspaces <- hasPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
  access <- if managesWorkspaces then return UnrestrictedWorkspaceAccess else workspaceAccess Nothing
  filter (isWorkspaceAccessible access . (.uuid)) <$> findWorkspaces

createWorkspace :: WizardRequestContextC s m => WorkspaceChangeDTO -> m Workspace
createWorkspace reqDto = do
  uuid <- liftIO generateUuid
  createWorkspaceWithUuid uuid reqDto

createWorkspaceWithUuid :: WizardRequestContextC s m => U.UUID -> WorkspaceChangeDTO -> m Workspace
createWorkspaceWithUuid uuid reqDto =
  runInTransaction $ do
    checkPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
    validateChangeDto reqDto
    tenantUuid <- asks (.tenantUuid')
    lockTenantByUuid tenantUuid
    tenant <- findTenantByUuid tenantUuid
    unless tenant.multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTimeInMicroseconds
    (workspace, adminRole) <- createWorkspaceWithRoles (fromChangeDTO reqDto uuid tenantUuid now)
    addWorkspaceMembership workspace currentUser.uuid (Just adminRole.uuid) now
    al_createWorkspace workspace
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
    now <- liftIO getCurrentTimeInMicroseconds
    let updatedWorkspace = fromChange workspace reqDto now
    updateWorkspaceByUuid updatedWorkspace
    al_updateWorkspace workspace updatedWorkspace
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
    whenAuditLogEnabled $ al_deleteWorkspace workspace =<< countWorkspaceDeletion uuid projectUuids editorUuids
    void $ deleteWorkspaceByUuid uuid
    traverse_ (removeWorkspaceLogo uuid) workspace.logo
    traverse_ logOutOnlineUsersWhenProjectDramaticallyChanged projectUuids
    traverse_ logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged editorUuids

getWorkspaceDeletion :: WizardRequestContextC s m => U.UUID -> m (Workspace, AuditLogWorkspaceDeleteData)
getWorkspaceDeletion uuid = do
  checkPermissionInWorkspace _WORKSPACES_MANAGE_ROLE_PERMISSION Nothing
  workspace <- findWorkspaceByUuid uuid
  projectUuids <- findProjectUuidsByWorkspaceUuid uuid
  editorUuids <- findKnowledgeModelEditorUuidsByWorkspaceUuid uuid
  (,) workspace <$> countWorkspaceDeletion uuid projectUuids editorUuids

countWorkspaceDeletion :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> [U.UUID] -> m AuditLogWorkspaceDeleteData
countWorkspaceDeletion uuid projectUuids editorUuids = do
  (documents, documentTemplateEditors, userGroups) <- countWorkspaceDependents uuid
  return $ AuditLogWorkspaceDeleteData (length projectUuids) documents (length editorUuids) documentTemplateEditors userGroups

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
    user <- findUserByUuid userUuid
    checkWorkspacePlaneAvailable
    now <- liftIO getCurrentTimeInMicroseconds
    (mOldMembership, membership) <- saveWorkspaceMembership workspace userUuid reqDto.roleUuid now
    notifyWorkspaceMemberSaved workspace user mOldMembership membership

removeWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeWorkspaceMember uuid userUuid =
  runInTransaction $ do
    checkPermissionInWorkspace _MEMBERS_MANAGE_ROLE_PERMISSION (Just uuid)
    when (userUuid == systemUserUuid) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SYSTEM_USER)
    workspace <- findWorkspaceByUuid uuid
    multiWorkspace <- asks (.tenantMultiWorkspace')
    unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
    mMembership <- findWorkspaceMembership' uuid userUuid
    removeWorkspaceMembership uuid userUuid
    traverse_ (al_removeWorkspaceMember workspace) mMembership
    traverse_ (notifyWorkspaceMemberRemoved workspace) mMembership

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
