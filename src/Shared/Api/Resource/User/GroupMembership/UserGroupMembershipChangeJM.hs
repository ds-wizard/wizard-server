module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipChangeDTO
import Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserGroupMembershipChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupMembershipChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserGroupMembershipChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
