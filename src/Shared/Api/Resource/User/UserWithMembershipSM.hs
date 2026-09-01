module Shared.Api.Resource.User.UserWithMembershipSM where

import Data.Swagger

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipSM ()
import Shared.Api.Resource.User.UserWithMembershipDTO
import Shared.Api.Resource.User.UserWithMembershipJM ()
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserWithMembershipDTO where
  declareNamedSchema = toSwagger userAlbertWithMembership
