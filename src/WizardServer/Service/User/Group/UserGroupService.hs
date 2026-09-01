module WizardServer.Service.User.Group.UserGroupService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
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
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Model.User.UserWithMembership
import Shared.Service.Project.Collaboration.ProjectCollaborationService
import Shared.Service.User.Group.UserGroupMapper
import WizardServer.Database.DAO.User.UserGroupDAO
import WizardServer.Model.User.UserGroupSuggestion
import WizardServer.Service.User.Group.UserGroupAcl
import WizardServer.Service.User.Group.UserGroupMapper

getUserGroupSuggestions :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page UserGroupSuggestion)
getUserGroupSuggestions = findUserGroupSuggestionsPage

createUserGroup :: WizardRequestContextC s m => U.UUID -> String -> Maybe String -> Bool -> m ()
createUserGroup uuid name description private = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let userGroup = fromCreate uuid name description private tenantUuid now
  insertUserGroup userGroup
  return ()

getUserGroupByUuid :: WizardRequestContextC s m => U.UUID -> m UserGroupDetailDTO
getUserGroupByUuid uuid = do
  userGroup <- findUserGroupByUuid uuid
  users <- findUsersByUserGroupUuid uuid
  checkViewPermission userGroup users
  return $ toDetailDTO userGroup users

modifyUserGroup :: WizardRequestContextC s m => U.UUID -> String -> Maybe String -> Bool -> m ()
modifyUserGroup uuid name description private = do
  userGroup <- findUserGroupByUuid uuid
  now <- liftIO getCurrentTime
  let updatedUserGroup = fromChange userGroup name description private now
  updateUserGroupByUuid updatedUserGroup
  return ()

deleteUserGroup :: WizardRequestContextC s m => U.UUID -> m ()
deleteUserGroup userGroupUuid =
  runInTransaction $ do
    -- 1. Recompute all project permissions for websockets
    projects <- findProjectsSimpleWithPermByUserGroupUuid userGroupUuid
    let projectsWithoutUserGroup = fmap (\project -> project {permissions = filter (\projectPerm -> projectPerm.memberUuid /= userGroupUuid) project.permissions}) projects
    traverse_ (\project -> updatePermsForOnlineUsers project.uuid project.visibility project.sharing project.permissions) projectsWithoutUserGroup
    -- 2. Delete project perm group
    deleteProjectCachesByUserGroupUuid userGroupUuid
    deleteProjectPermGroupByUserGroupUuid userGroupUuid
    -- 3. Delete user group memberships
    deleteUserGroupMembershipsByUserGroupUuid userGroupUuid
    -- 4. Delete user group
    deleteUserGroupByUuid userGroupUuid
