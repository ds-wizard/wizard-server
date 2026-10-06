module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.User.UserGroupMembership

data UserGroupMembershipChangeDTO = UserGroupMembershipChangeDTO
  { userUuid :: U.UUID
  , mType :: UserGroupMembershipType
  }
  deriving (Show, Eq, Generic)
