module Shared.Database.Mapping.Plugin.PluginList where

import Database.PostgreSQL.Simple

import Shared.Model.Plugin.PluginList

instance ToRow PluginList

instance FromRow PluginList
