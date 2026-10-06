module Shared.Database.Mapping.Settings.SettingsCustomMenuLink where

import Database.PostgreSQL.Simple

import Shared.Model.Settings.Settings

instance ToRow SettingsCustomMenuLink

instance FromRow SettingsCustomMenuLink
