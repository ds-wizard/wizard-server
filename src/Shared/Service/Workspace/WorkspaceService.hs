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
import Shared.Constant.User
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Project.ProjectDAO
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
import Shared.Model.Workspace.Workspace
import Shared.Model.Workspace.WorkspaceMember
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.Workspace.WorkspaceMapper
import Shared.Service.Workspace.WorkspaceMembershipService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.String (trim)
import Shared.Util.Uuid

getWorkspacesPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page Workspace)
getWorkspacesPage mQuery pageable sort = do
  currentUser <- getCurrentUser
  findWorkspacesPage currentUser.uuid mQuery pageable sort

createWorkspace :: WizardRequestContextC s m => WorkspaceChangeDTO -> m Workspace
createWorkspace reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    validateChangeDto reqDto
    multiWorkspace <- asks (.tenantMultiWorkspace')
    unless multiWorkspace (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SINGLE_WORKSPACE_TENANT)
    currentUser <- getCurrentUser
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    workspace <- insertWorkspaceWithSystemUser (fromChangeDTO reqDto uuid tenantUuid now)
    addWorkspaceMembership workspace.uuid currentUser.uuid tenantUuid now
    return workspace

createDefaultWorkspace :: WizardRequestContextC s m => U.UUID -> String -> UTCTime -> m Workspace
createDefaultWorkspace tenantUuid name now = do
  uuid <- liftIO generateUuid
  insertWorkspaceWithSystemUser (fromChangeDTO (WorkspaceChangeDTO {name = name, description = Nothing}) uuid tenantUuid now)

getWorkspaceByUuid :: WizardRequestContextC s m => U.UUID -> m Workspace
getWorkspaceByUuid uuid = do
  checkWorkspaceMembership uuid
  findWorkspaceByUuid uuid

modifyWorkspace :: WizardRequestContextC s m => U.UUID -> WorkspaceChangeDTO -> m Workspace
modifyWorkspace uuid reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    validateChangeDto reqDto
    workspace <- findWorkspaceByUuid uuid
    now <- liftIO getCurrentTime
    let updatedWorkspace = fromChange workspace reqDto now
    updateWorkspaceByUuid updatedWorkspace
    return updatedWorkspace

deleteWorkspace :: WizardRequestContextC s m => U.UUID -> m ()
deleteWorkspace uuid =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    _ <- findWorkspaceByUuid uuid
    workspaceUuids <- findWorkspaceUuidsForUpdate
    when (length workspaceUuids < 2) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__LAST_WORKSPACE)
    projectUuids <- findProjectUuidsByWorkspaceUuid uuid
    editorUuids <- findKnowledgeModelEditorUuidsByWorkspaceUuid uuid
    void $ deleteWorkspaceByUuid uuid
    traverse_ logOutOnlineUsersWhenProjectDramaticallyChanged projectUuids
    traverse_ logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged editorUuids

getWorkspaceMembersPage :: WizardRequestContextC s m => U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page WorkspaceMember)
getWorkspaceMembersPage uuid mQuery pageable sort = do
  checkWorkspaceMembership uuid
  findWorkspaceMembersPage uuid mQuery pageable sort

addWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
addWorkspaceMember uuid userUuid =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    workspace <- findWorkspaceByUuid uuid
    _ <- findUserByUuid userUuid
    now <- liftIO getCurrentTime
    addWorkspaceMembership workspace.uuid userUuid workspace.tenantUuid now

removeWorkspaceMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
removeWorkspaceMember uuid userUuid =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    when (userUuid == systemUserUuid) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__SYSTEM_USER)
    _ <- findWorkspaceByUuid uuid
    removeWorkspaceMembership uuid userUuid

insertWorkspaceWithSystemUser :: WizardRequestContextC s m => Workspace -> m Workspace
insertWorkspaceWithSystemUser workspace = do
  insertWorkspace workspace
  addWorkspaceMembership workspace.uuid systemUserUuid workspace.tenantUuid workspace.createdAt
  return workspace

validateChangeDto :: WizardRequestContextC s m => WorkspaceChangeDTO -> m ()
validateChangeDto reqDto =
  when (null (trim reqDto.name)) (throwError $ ValidationError [] (M.singleton "name" [_ERROR_VALIDATION__FIELDS_ABSENCE]))
