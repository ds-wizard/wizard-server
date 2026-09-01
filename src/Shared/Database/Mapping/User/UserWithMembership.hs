module Shared.Database.Mapping.User.UserWithMembership where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.User.UserGroupMembership ()
import Shared.Model.User.UserWithMembership

instance FromRow UserWithMembership where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    imageUrl <- field
    affiliation <- field
    membershipType <- field
    return $ UserWithMembership {..}
