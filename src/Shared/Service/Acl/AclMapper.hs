module Shared.Service.Acl.AclMapper where

import Shared.Api.Resource.Acl.MemberDTO
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Util.Gravatar

toUserMemberDTO :: User -> MemberDTO
toUserMemberDTO user =
  UserMemberDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash user.email
    , imageUrl = user.imageUrl
    , affiliation = user.affiliation
    }

toUserGroupMemberDTO :: UserGroup -> MemberDTO
toUserGroupMemberDTO userGroup =
  UserGroupMemberDTO
    { uuid = userGroup.uuid
    , name = userGroup.name
    , description = userGroup.description
    , private = userGroup.private
    }
