module Shared.Database.Mapping.User.UserGroupList where

import Database.PostgreSQL.Simple

import Shared.Model.User.UserGroupList

instance FromRow UserGroupList
