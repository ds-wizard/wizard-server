module Shared.Service.User.Group.UserGroupMapper where

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Model.User.UserGroup
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
    }
