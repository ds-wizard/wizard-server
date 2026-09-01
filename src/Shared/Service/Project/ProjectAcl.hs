module Shared.Service.Project.ProjectAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

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
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.UserGroupMembership
import Shared.Service.Tenant.Config.ConfigService

checkCreatePermissionToProject :: WizardRequestContextC s m => m ()
checkCreatePermissionToProject = do
  tcProject <- getCurrentTenantConfigProject
  let projectSharingEnabled = tcProject.projectSharing.enabled
  let projectSharingAnonymousEnabled = tcProject.projectSharing.anonymousEnabled
  let projectCreation = tcProject.projectCreation
  case (projectSharingEnabled, projectSharingAnonymousEnabled, projectCreation) of
    (True, True, CustomProjectCreation) -> return ()
    (True, True, TemplateAndCustomProjectCreation) -> return ()
    (_, _, TemplateProjectCreation) -> checkPermission _PROJECT_TEMPLATES_MANAGE_ROLE_PERMISSION
    (_, _, _) -> return ()

checkCreateFromTemplatePermissionToProject :: WizardRequestContextC s m => Bool -> m ()
checkCreateFromTemplatePermissionToProject isTemplate = do
  tcProject <- getCurrentTenantConfigProject
  let projectCreation = tcProject.projectCreation
  case projectCreation of
    CustomProjectCreation ->
      throwError . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Project Template"
    _ -> unless isTemplate (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Project Template")

checkClonePermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkClonePermissionToProject visibility sharing permissions = do
  checkViewPermissionToProject visibility sharing permissions

checkViewPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkViewPermissionToProject visibility sharing perms = do
  result <- hasViewPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"

hasViewPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasViewPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkViewProjectSharing
    || sharing == AnyoneWithLinkCommentProjectSharing
    || sharing
      == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_VIEW_ROLE_PERMISSION
      if or
        [ hasPermission
        , -- Check visibility
          visibility == VisibleViewProjectVisibility
        , visibility == VisibleCommentProjectVisibility
        , visibility == VisibleEditProjectVisibility
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

checkCommentPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkCommentPermissionToProject visibility sharing perms = do
  result <- hasCommentPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Comment Project"

hasCommentPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasCommentPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkCommentProjectSharing || sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_COMMENT_ROLE_PERMISSION
      if or
        [ hasPermission
        , -- Check visibility
          visibility == VisibleCommentProjectVisibility
        , visibility == VisibleEditProjectVisibility
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

checkEditPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m ()
checkEditPermissionToProject visibility sharing perms = do
  result <- hasEditPermissionToProject visibility sharing perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Project"

hasEditPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> ProjectSharing -> [projectPerm] -> m Bool
hasEditPermissionToProject visibility sharing perms =
  if sharing == AnyoneWithLinkEditProjectSharing
    then return True
    else do
      currentUser <- getCurrentUser
      userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
      let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
      hasPermission <- hasPermission _PROJECTS_EDIT_ROLE_PERMISSION
      if or
        [ hasPermission
        , -- Check visibility
          visibility == VisibleEditProjectVisibility
        , -- Check membership
          currentUser.uuid `elem` getUserUuidsForEditorPerm perms
        , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
        , -- Check groups
          any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
        , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
        ]
        then return True
        else return False

checkOwnerPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> [projectPerm] -> m ()
checkOwnerPermissionToProject visibility perms = do
  result <- hasOwnerPermissionToProject visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Administrate Project"

hasOwnerPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> [projectPerm] -> m Bool
hasOwnerPermissionToProject visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  hasPermission <- hasPermission _PROJECTS_MANAGE_ROLE_PERMISSION
  if or
    [ hasPermission
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
    ]
    then return True
    else return False

checkMigrationPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> [projectPerm] -> m ()
checkMigrationPermissionToProject visibility perms = do
  result <- hasMigrationPermissionToProject visibility perms
  if result
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Migrate Project"

hasMigrationPermissionToProject :: (WizardRequestContextC s m, ProjectPermC projectPerm) => ProjectVisibility -> [projectPerm] -> m Bool
hasMigrationPermissionToProject visibility perms = do
  currentUser <- getCurrentUser
  userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
  let currentUserGroupUuids = fmap (.userGroupUuid) userGroupMemberships
  hasPermission <- hasPermission _PROJECTS_EDIT_ROLE_PERMISSION
  if or
    [ hasPermission
    , -- Check visibility
      visibility == VisibleEditProjectVisibility
    , -- Check membership
      currentUser.uuid `elem` getUserUuidsForEditorPerm perms
    , currentUser.uuid `elem` getUserUuidsForOwnerPerm perms
    , -- Check groups
      any (`elem` getUserGroupUuidsForEditorPerm perms) currentUserGroupUuids
    , any (`elem` getUserGroupUuidsForOwnerPerm perms) currentUserGroupUuids
    ]
    then return True
    else return False
