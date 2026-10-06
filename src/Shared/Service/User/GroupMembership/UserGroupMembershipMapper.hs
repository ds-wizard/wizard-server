module Shared.Service.User.GroupMembership.UserGroupMembershipMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Model.User.UserGroupMembership

toCreateDTO :: UserGroupMembership -> UserGroupMembershipChangeDTO
toCreateDTO membership =
  UserGroupMembershipChangeDTO
    { userUuid = membership.userUuid
    , mType = membership.mType
    }

fromCreate :: U.UUID -> U.UUID -> UTCTime -> U.UUID -> UserGroupMembership
fromCreate userUuid tenantUuid now userGroupUuid =
  UserGroupMembership
    { userGroupUuid = userGroupUuid
    , userUuid = userUuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromCreateDTO :: UserGroupMembershipChangeDTO -> U.UUID -> U.UUID -> UTCTime -> UserGroupMembership
fromCreateDTO dto userGroupUuid tenantUuid now =
  UserGroupMembership
    { userGroupUuid = userGroupUuid
    , userUuid = dto.userUuid
    , mType = dto.mType
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: UserGroupMembership -> UserGroupMembershipChangeDTO -> UTCTime -> UserGroupMembership
fromChangeDTO membership dto now =
  UserGroupMembership
    { userGroupUuid = membership.userGroupUuid
    , userUuid = dto.userUuid
    , mType = dto.mType
    , tenantUuid = membership.tenantUuid
    , createdAt = membership.createdAt
    , updatedAt = now
    }

fromDefaultOwner :: U.UUID -> UserDTO -> U.UUID -> UTCTime -> UserGroupMembership
fromDefaultOwner userGroupUuid user tenantUuid now =
  UserGroupMembership
    { userGroupUuid = userGroupUuid
    , userUuid = user.uuid
    , mType = OwnerUserGroupMembershipType
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }
