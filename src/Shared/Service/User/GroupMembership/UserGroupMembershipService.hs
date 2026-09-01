module Shared.Service.User.GroupMembership.UserGroupMembershipService where

import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.ProjectSimpleWithPerm
import Shared.Service.Project.Collaboration.ProjectCollaborationService

removeUserGroupMembersFromOnlineUsers :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m ()
removeUserGroupMembersFromOnlineUsers userGroupUuid userUuids = do
  removeUserGroupFromUsers userGroupUuid userUuids
  projects <- findProjectsSimpleWithPermByUserGroupUuid userGroupUuid
  traverse_ (\project -> updatePermsForOnlineUsers project.uuid project.visibility project.sharing project.permissions) projects
