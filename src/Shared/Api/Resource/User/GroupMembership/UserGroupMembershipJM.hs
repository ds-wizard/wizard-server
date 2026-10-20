module Shared.Api.Resource.User.GroupMembership.UserGroupMembershipJM where

import Data.Aeson

import Shared.Model.User.UserGroupMembership
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserGroupMembershipType where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupMembershipType where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserGroupMembershipType where
  jsonSchema = genericJsonSchema jsonOptions
