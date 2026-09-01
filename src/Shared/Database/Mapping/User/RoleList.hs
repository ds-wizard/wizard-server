module Shared.Database.Mapping.User.RoleList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Model.User.RoleList

instance FromRow RoleList where
  fromRow = do
    uuid <- field
    name <- field
    permissions <- fromPGArray <$> field
    usersCount <- field
    isAdmin <- field
    return $ RoleList {..}
