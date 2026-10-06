module Shared.Service.User.GroupMembership.UserGroupMembershipService where

import Control.Monad (unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.User.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupChange
import Shared.Model.User.UserGroupMembership
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.User.Group.UserGroupAcl
import Shared.Service.User.GroupMembership.UserGroupMembershipMapper
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.List

createDefaultOwnerMembership :: WizardRequestContextC s m => U.UUID -> m ()
createDefaultOwnerMembership userGroupUuid = do
  user <- getCurrentUser
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  void $ insertUserGroupMembership (fromDefaultOwner userGroupUuid user tenantUuid now)

modifyUserGroupMembership :: WizardRequestContextC s m => (UserGroupChange -> m ()) -> U.UUID -> [UserGroupMembershipChangeDTO] -> m [UserGroupMembershipChangeDTO]
modifyUserGroupMembership auditFn userGroupUuid members =
  runInTransaction $ do
    checkDuplicateMembers members
    userGroup <- findUserGroupByUuid userGroupUuid
    checkEditPermission userGroup
    oldMembers <- findUsersByUserGroupUuid userGroupUuid
    memberships <- findUserGroupMembershipsByUserGroupUuid userGroupUuid
    let userUuidsToDelete = filter (`notElem` fmap (.userUuid) members) (fmap (.userUuid) memberships)
    applyMembershipChanges userGroup memberships members
    unless (null userUuidsToDelete) (deleteUserGroupMembershipsByUserGroupUuidAndUserUuids userGroupUuid userUuidsToDelete)
    newMembers <- findUsersByUserGroupUuid userGroupUuid
    now <- liftIO getCurrentTime
    updateUserGroupByUuid $ userGroup {updatedAt = now}
    auditFn (UserGroupMembersUpdated userGroup oldMembers newMembers)
    unless (null userUuidsToDelete) (removeUserGroupMembersFromOnlineUsers userGroupUuid userUuidsToDelete)
    return members

applyMembershipChanges :: WizardRequestContextC s m => UserGroup -> [UserGroupMembership] -> [UserGroupMembershipChangeDTO] -> m ()
applyMembershipChanges userGroup memberships members = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let membershipsToAdd = filter ((`notElem` fmap (.userUuid) memberships) . (.userUuid)) members
  let membershipsToEdit = filter (\(membership, dto) -> membership.mType /= dto.mType) (groupBy' (.userUuid) (.userUuid) memberships members)
  traverse_ (checkWorkspaceMembershipOfMember userGroup.workspaceUuid . (.userUuid)) membershipsToAdd
  traverse_ (insertUserGroupMembership . (\dto -> fromCreateDTO dto userGroup.uuid tenantUuid now)) membershipsToAdd
  traverse_ (updateUserGroupMembershipByUuid . (\(membership, dto) -> fromChangeDTO membership dto now)) membershipsToEdit

checkDuplicateMembers :: WizardRequestContextC s m => [UserGroupMembershipChangeDTO] -> m ()
checkDuplicateMembers members =
  let userUuids = fmap (.userUuid) members
   in when (length (L.nub userUuids) /= length userUuids) (throwError $ UserError _ERROR_SERVICE_USER_GROUP__DUPLICATE_MEMBER)

checkWorkspaceMembershipOfMember :: WizardRequestContextC s m => U.UUID -> U.UUID -> m ()
checkWorkspaceMembershipOfMember workspaceUuid userUuid = do
  isMember <- isWorkspaceMember workspaceUuid userUuid
  unless isMember (throwError $ UserError _ERROR_SERVICE_WORKSPACE__NOT_MEMBER)

removeUserGroupMembersFromOnlineUsers :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroupMembersFromOnlineUsers userGroupUuid userUuids = do
  removeUserGroupFromUsers userGroupUuid userUuids
  projects <- findProjectsSimpleWithPermByUserGroupUuid userGroupUuid
  traverse_ (\project -> updatePermsForOnlineUsers project.uuid project.workspaceUuid project.visibility project.sharing project.permissions) projects
