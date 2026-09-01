module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipSM where

import Data.Swagger

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import Shared.Model.User.UserGroupMembership
import Shared.Util.Swagger

instance ToSchema UserGroupMembershipType where
  declareNamedSchema = toSwagger OwnerUserGroupMembershipType
