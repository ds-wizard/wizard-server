module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeSM where

import Data.Swagger

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeJM ()
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipSM ()
import Shared.Model.User.UserGroupMembership
import Shared.Util.Swagger
import Shared.Util.Uuid

instance ToSchema UserGroupMembershipChangeDTO where
  declareNamedSchema =
    toSwagger
      ( UserGroupMembershipChangeDTO
          { userUuid = u' "fc768ea8-a077-4fa5-82da-b27e07e7ba22"
          , mType = OwnerUserGroupMembershipType
          }
      )
