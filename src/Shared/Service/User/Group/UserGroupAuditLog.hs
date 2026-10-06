module Shared.Service.User.Group.UserGroupAuditLog where

import Control.Monad (unless)

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.UserGroupAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserGroup
import Shared.Model.User.UserWithMembership
import Shared.Service.AuditLog.AuditLogMapper (toUserRef)
import Shared.Service.AuditLog.AuditLogService
import Shared.Util.List

al_createUserGroup :: WizardRequestContextC s m => UserGroup -> m ()
al_createUserGroup userGroup = logAuditLog userGroupsCreate (userGroupEntity userGroup) (toUserGroupData userGroup)

al_updateUserGroup :: WizardRequestContextC s m => UserGroup -> UserGroup -> m ()
al_updateUserGroup userGroup updatedUserGroup =
  logAuditLogChanges
    userGroupsUpdate
    (userGroupEntity updatedUserGroup)
    AuditLogUserGroupChanges
      { name = change userGroup.name updatedUserGroup.name
      , description = change userGroup.description updatedUserGroup.description
      , private = change userGroup.private updatedUserGroup.private
      }

al_deleteUserGroup :: WizardRequestContextC s m => UserGroup -> m ()
al_deleteUserGroup userGroup = logAuditLog userGroupsDelete (userGroupEntity userGroup) (toUserGroupData userGroup)

al_updateMembers :: WizardRequestContextC s m => UserGroup -> [UserWithMembership] -> [UserWithMembership] -> m ()
al_updateMembers userGroup oldMembers newMembers = do
  let members = toMembersData oldMembers newMembers
  unless (null members.add && null members.edit && null members.delete) $
    logAuditLog userGroupsUpdateMembers (userGroupEntity userGroup) members

toMembersData :: [UserWithMembership] -> [UserWithMembership] -> AuditLogUserGroupMembersData
toMembersData oldMembers newMembers =
  AuditLogUserGroupMembersData
    { add = fmap toMember (filter ((`notElem` fmap (.uuid) oldMembers) . (.uuid)) newMembers)
    , edit = [toMemberEdit old new | (old, new) <- groupBy' (.uuid) (.uuid) oldMembers newMembers, old.membershipType /= new.membershipType]
    , delete = fmap toMember (filter ((`notElem` fmap (.uuid) newMembers) . (.uuid)) oldMembers)
    }

userGroupEntity :: UserGroup -> AuditLogEntity
userGroupEntity userGroup = entityInWorkspace userGroup.workspaceUuid userGroup.uuid userGroup.name

toUserGroupData :: UserGroup -> AuditLogUserGroupData
toUserGroupData userGroup = AuditLogUserGroupData {description = userGroup.description, private = userGroup.private}

toMember :: UserWithMembership -> AuditLogUserGroupMember
toMember member = AuditLogUserGroupMember {user = toUserRef member, membershipType = show member.membershipType}

toMemberEdit :: UserWithMembership -> UserWithMembership -> AuditLogUserGroupMemberEdit
toMemberEdit old new =
  AuditLogUserGroupMemberEdit {user = toUserRef new, membershipType = Change {old = show old.membershipType, new = show new.membershipType}}
