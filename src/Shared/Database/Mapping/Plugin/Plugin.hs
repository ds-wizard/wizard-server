module Shared.Database.Mapping.Plugin.Plugin where

import Database.PostgreSQL.Simple

import Shared.Model.Plugin.Plugin

instance ToRow Plugin

instance FromRow Plugin
