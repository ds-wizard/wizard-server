module Shared.Database.Mapping.User.UserGroupMembership where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.User.UserGroupMembership

instance ToField UserGroupMembershipType where
  toField = toFieldGenericEnum

instance FromField UserGroupMembershipType where
  fromField = fromFieldGenericEnum

instance ToRow UserGroupMembership

instance FromRow UserGroupMembership
