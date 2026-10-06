module Shared.Service.User.Group.UserGroupAudit where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupChange
import Shared.Model.User.UserWithMembership
import Shared.Service.Audit.AuditService

auditUserGroupChange :: WizardRequestContextC s m => UserGroupChange -> m ()
auditUserGroupChange change =
  logAuditWithBody "userGroup" (userGroupChangeAction change) (U.toString (userGroupChangeGroup change).uuid) (userGroupChangeBody change)

userGroupChangeAction :: UserGroupChange -> String
userGroupChangeAction UserGroupCreated {} = "create"
userGroupChangeAction UserGroupUpdated {} = "update"
userGroupChangeAction UserGroupDeleted {} = "delete"
userGroupChangeAction UserGroupMembersUpdated {} = "updateMembers"

userGroupChangeGroup :: UserGroupChange -> UserGroup
userGroupChangeGroup (UserGroupCreated userGroup) = userGroup
userGroupChangeGroup (UserGroupUpdated _ userGroup) = userGroup
userGroupChangeGroup (UserGroupDeleted userGroup) = userGroup
userGroupChangeGroup (UserGroupMembersUpdated userGroup _ _) = userGroup

userGroupChangeBody :: UserGroupChange -> M.Map String String
userGroupChangeBody (UserGroupMembersUpdated userGroup _ newMembers) =
  M.fromList [("name", userGroup.name), ("members", L.intercalate "," (fmap memberEntry newMembers))]
userGroupChangeBody change =
  let userGroup = userGroupChangeGroup change
   in M.fromList [("name", userGroup.name), ("workspaceUuid", U.toString userGroup.workspaceUuid)]

memberEntry :: UserWithMembership -> String
memberEntry member = U.toString member.uuid <> ":" <> show member.membershipType
