module Shared.Service.Project.ProjectAcl where

import Control.Monad (forM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.Acl.ProjectAclHelpers
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.User.UserGroupMembership
import Shared.Service.Settings.WorkspaceSettingsService
import Shared.Service.Workspace.WorkspaceScopeService

checkCreatePermissionToProject :: WizardRequestContextC s m => U.UUID -> m ()
checkCreatePermissionToProject workspaceUuid = do
  mCurrentUser <- asks (.currentUser')
  forM_ mCurrentUser (const (checkPermission _PROJECTS_CREATE_ROLE_PERMISSION))
  tcProject <- getEffectiveSettingsProjects (Just workspaceUuid)
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  let projectCreation = tcProject.projectCreation
  case (projectSharingEnabled, projectSharingAnonymousEnabled, projectCreation) of
    (True, True, CustomProjectCreation) -> return ()
    (True, True, TemplateAndCustomProjectCreation) -> return ()
    (_, _, TemplateProjectCreation) -> checkPermission _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
    (_, _, _) -> return ()

checkCreateFromTemplatePermissionToProject :: WizardRequestContextC s m => U.UUID -> Bool -> m ()
checkCreateFromTemplatePermissionToProject workspaceUuid isTemplate = do
  checkPermissionInWorkspace _PROJECTS_CREATE_ROLE_PERMISSION (Just workspaceUuid)
  tcProject <- getEffectiveSettingsProjects (Just workspaceUuid)
  let projectCreation = tcProject.projectCreation
  case projectCreation of
    CustomProjectCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template")

checkClonePermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkClonePermissionToProject workspaceUuid visibility sharing permissions = do
  checkPermissionInWorkspace _PROJECTS_CREATE_ROLE_PERMISSION (Just workspaceUuid)
  checkViewPermissionToProject workspaceUuid visibility sharing permissions

checkViewPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkViewPermissionToProject workspaceUuid visibility sharing perms = do
  result <- hasViewPermissionToProject workspaceUuid visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"

hasViewPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasViewPermissionToProject workspaceUuid visibility sharing perms =
  if sharing == AnyoneWithLinkViewProjectSharing
    || sharing == AnyoneWithLinkCommentProjectSharing
    || sharing
      == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermissionInWorkspace _PROJECTS_VIEW_ROLE_PERMISSION (Just workspaceUuid)
      reachable <- isWorkspaceReachable workspaceUuid
      if or
        [ hasPermission
        , -- Check visibility
          reachable && visibility == VisibleViewProjectVisibility
        , reachable && visibility == VisibleCommentProjectVisibility
        , reachable && visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForViewerPerm perms
        , currentUser.uuid `elem` getUserUuidsForCommenterPerm perms
        , currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForViewerPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
        ]
        then return True
        else return False

checkCommentPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkCommentPermissionToProject workspaceUuid visibility sharing perms = do
  result <- hasCommentPermissionToProject workspaceUuid visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Project"

hasCommentPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasCommentPermissionToProject workspaceUuid visibility sharing perms =
  if sharing == AnyoneWithLinkCommentProjectSharing || sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermissionInWorkspace _PROJECTS_COMMENT_ROLE_PERMISSION (Just workspaceUuid)
      reachable <- isWorkspaceReachable workspaceUuid
      if or
        [ hasPermission
        , -- Check visibility
          reachable && visibility == VisibleCommentProjectVisibility
        , reachable && visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForCommenterPerm perms
        , currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForCommenterPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
        ]
        then return True
        else return False

checkEditPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkEditPermissionToProject workspaceUuid visibility sharing perms = do
  result <- hasEditPermissionToProject workspaceUuid visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Project"

hasEditPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasEditPermissionToProject workspaceUuid visibility sharing perms =
  if sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermissionInWorkspace _PROJECTS_EDIT_ROLE_PERMISSION (Just workspaceUuid)
      reachable <- isWorkspaceReachable workspaceUuid
      if or
        [ hasPermission
        , -- Check visibility
          reachable && visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
        ]
        then return True
        else return False

checkOwnerPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> [projectPerm] -> m ()
checkOwnerPermissionToProject workspaceUuid visibility perms = do
  result <- hasOwnerPermissionToProject workspaceUuid visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Project"

hasOwnerPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> [projectPerm] -> m Bool
hasOwnerPermissionToProject workspaceUuid visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  hasPermission <- hasPermissionInWorkspace _PROJECTS_MANAGE_ROLE_PERMISSION (Just workspaceUuid)
  if or
    [ hasPermission
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
    ]
    then return True
    else return False

checkMigrationPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> [projectPerm] -> m ()
checkMigrationPermissionToProject workspaceUuid visibility perms = do
  result <- hasMigrationPermissionToProject workspaceUuid visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Project"

hasMigrationPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => U.UUID -> ProjectVisibility -> [projectPerm] -> m Bool
hasMigrationPermissionToProject workspaceUuid visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  hasPermission <- hasPermissionInWorkspace _PROJECTS_EDIT_ROLE_PERMISSION (Just workspaceUuid)
  reachable <- isWorkspaceReachable workspaceUuid
  if or
    [ hasPermission
    , -- Check visibility
      reachable && visibility == VisibleEditProjectVisibility
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForEditorPerm perms
    , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
    , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
    ]
    then return True
    else return False
