module Shared.Service.User.Group.UserGroupMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupList
import Shared.Model.User.UserGroupSuggestion
import Shared.Model.User.UserWithMembership
import Shared.Service.User.UserMapper

toDetailDTO :: UserGroup -> [UserWithMembership] -> UserGroupDetailDTO
toDetailDTO userGroup users =
  UserGroupDetailDTO
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    , users = fmap toWithMembershipDTO users
    , createdAt = userGroup.createdAt
    , updatedAt = userGroup.updatedAt
    , workspaceUuid = userGroup.workspaceUuid
    }

toList :: UserGroup -> Bool -> Bool -> UserGroupList
toList userGroup editable member =
  UserGroupList
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    , editable = editable
    , member = member
    , updatedAt = userGroup.updatedAt
    , workspaceUuid = userGroup.workspaceUuid
    }

toSuggestion :: UserGroup -> UserGroupSuggestion
toSuggestion userGroup =
  UserGroupSuggestion
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    }

toChangeDTO :: UserGroup -> UserGroupChangeDTO
toChangeDTO userGroup =
  UserGroupChangeDTO
    { name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    }

fromCreateDTO :: UserGroupChangeDTO -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> UserGroup
fromCreateDTO dto uuid tenantUuid workspaceUuid now =
  UserGroup
    { uuid = uuid
    , name = dto.name
    , description = dto.description
    , private = dto.private
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    , workspaceUuid = workspaceUuid
    }

fromChangeDTO :: UserGroup -> UserGroupChangeDTO -> UTCTime -> UserGroup
fromChangeDTO userGroup dto now =
  userGroup
    { name = dto.name
    , description = dto.description
    , private = dto.private
    , updatedAt = now
    }
