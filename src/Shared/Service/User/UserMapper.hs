module Shared.Service.User.UserMapper where

import Shared.Api.Resource.User.UserWithMembershipDTO
import Shared.Model.User.UserWithMembership
import Shared.Util.Gravatar (createGravatarHash)

toWithMembershipDTO :: UserWithMembership -> UserWithMembershipDTO
toWithMembershipDTO user =
  UserWithMembershipDTO
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = createGravatarHash user.email
    , imageUrl = user.imageUrl
    , affiliation = user.affiliation
    , membershipType = user.membershipType
    }
