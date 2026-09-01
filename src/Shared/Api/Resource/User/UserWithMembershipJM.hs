module Shared.Api.Resource.User.UserWithMembershipJM where

import Data.Aeson

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import Shared.Api.Resource.User.UserWithMembershipDTO
import Shared.Util.Aeson

instance FromJSON UserWithMembershipDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserWithMembershipDTO where
  toJSON = genericToJSON jsonOptions
