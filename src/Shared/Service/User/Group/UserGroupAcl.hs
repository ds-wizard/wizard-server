module Shared.Service.User.Group.UserGroupAcl where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Localization.Messages.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import Shared.Service.Workspace.WorkspaceScopeService

checkCreatePermission :: WizardRequestContextC s m => U.UUID -> m ()
checkCreatePermission workspaceUuid = checkPermissionInWorkspace _USER_GROUPS_CREATE_ROLE_PERMISSION (Just workspaceUuid)

checkViewPermission :: (WizardRequestContextC s m, HasField "uuid" user U.UUID) => UserGroup -> [user] -> m ()
checkViewPermission userGroup users = do
  currentUser <- getCurrentUser
  let userUuids = fmap (.uuid) users
  canManage <- hasPermissionInWorkspace _USER_GROUPS_MANAGE_ROLE_PERMISSION (Just userGroup.workspaceUuid)
  reachable <- isWorkspaceReachable userGroup.workspaceUuid
  unless
    (canManage || (reachable && not userGroup.private) || currentUser.uuid `elem` userUuids)
    (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View UserGroup")

checkEditPermission :: WizardRequestContextC s m => UserGroup -> m ()
checkEditPermission userGroup = do
  currentUser <- getCurrentUser
  canManage <- hasPermissionInWorkspace _USER_GROUPS_MANAGE_ROLE_PERMISSION (Just userGroup.workspaceUuid)
  unless canManage $ do
    mMembership <- findUserGroupMembershipByUserGroupUuidAndUserUuid' userGroup.uuid currentUser.uuid
    unless
      (fmap (.mType) mMembership == Just OwnerUserGroupMembershipType)
      (throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit UserGroup")
