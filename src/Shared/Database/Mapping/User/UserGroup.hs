module Shared.Database.Mapping.User.UserGroup where

import Database.PostgreSQL.Simple

import Shared.Model.User.UserGroup

instance ToRow UserGroup

instance FromRow UserGroup
