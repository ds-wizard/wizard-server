module Shared.Database.Mapping.User.UserGroupSuggestion where

import Database.PostgreSQL.Simple

import Shared.Model.User.UserGroupSuggestion

instance FromRow UserGroupSuggestion
