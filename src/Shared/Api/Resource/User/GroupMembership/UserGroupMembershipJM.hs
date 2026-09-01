module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM where

import Data.Aeson

import Shared.Model.User.UserGroupMembership
import Shared.Util.Aeson

instance FromJSON UserGroupMembershipType where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupMembershipType where
  toJSON = genericToJSON jsonOptions
