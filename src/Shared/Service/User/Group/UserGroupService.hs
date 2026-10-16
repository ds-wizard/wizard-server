module Shared.Service.User.Group.UserGroupService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectPermDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupList
import Shared.Model.User.UserGroupMembership
import Shared.Model.User.UserGroupSuggestion
import Shared.Model.User.UserWithMembership
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.User.Group.UserGroupAcl
import Shared.Service.User.Group.UserGroupAuditLog
import Shared.Service.User.Group.UserGroupMapper
import Shared.Service.User.GroupMembership.UserGroupMembershipService
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Uuid

getUserGroupsPage :: WizardRequestContextC s m => Maybe String -> Maybe Bool -> Pageable -> [Sort] -> m (Page UserGroupList)
getUserGroupsPage = findUserGroupsPage

getUserGroupSuggestions :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page UserGroupSuggestion)
getUserGroupSuggestions = findUserGroupSuggestionsPage

getUserGroupByUuid :: WizardRequestContextC s m => U.UUID -> m UserGroupDetailDTO
getUserGroupByUuid uuid = do
  userGroup <- findUserGroupByUuid uuid
  users <- findUsersByUserGroupUuid uuid
  checkViewPermission userGroup users
  return $ toDetailDTO userGroup users

createUserGroup :: WizardRequestContextC s m => UserGroupChangeDTO -> m UserGroup
createUserGroup reqDto =
  runInTransaction $ do
    workspaceUuid <- requireWorkspaceScope
    checkCreatePermission workspaceUuid
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let userGroup = fromCreateDTO reqDto uuid tenantUuid workspaceUuid now
    insertUserGroup userGroup
    currentUser <- getCurrentUser
    isMember <- isWorkspaceMember workspaceUuid currentUser.uuid
    when isMember (createDefaultOwnerMembership uuid)
    al_createUserGroup userGroup
    return userGroup

modifyUserGroup :: WizardRequestContextC s m => U.UUID -> UserGroupChangeDTO -> m UserGroup
modifyUserGroup uuid reqDto =
  runInTransaction $ do
    userGroup <- findUserGroupByUuid uuid
    checkEditPermission userGroup
    now <- liftIO getCurrentTime
    let updatedUserGroup = fromChangeDTO userGroup reqDto now
    updateUserGroupByUuid updatedUserGroup
    al_updateUserGroup userGroup updatedUserGroup
    return updatedUserGroup

deleteUserGroup :: WizardRequestContextC s m => U.UUID -> m ()
deleteUserGroup uuid =
  runInTransaction $ do
    userGroup <- findUserGroupByUuid uuid
    checkEditPermission userGroup
    memberships <- findUserGroupMembershipsByUserGroupUuid uuid
    projects <- findProjectsSimpleWithPermByUserGroupUuid uuid
    deleteProjectCachesByUserGroupUuid uuid
    deleteProjectPermGroupByUserGroupUuid uuid
    deleteUserGroupMembershipsByUserGroupUuid uuid
    deleteUserGroupByUuid uuid
    al_deleteUserGroup userGroup
    removeUserGroupFromUsers uuid (fmap (.userUuid) memberships)
    traverse_ (updatePermsForOnlineUsers . (.uuid)) projects
