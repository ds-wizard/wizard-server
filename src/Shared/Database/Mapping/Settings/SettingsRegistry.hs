module Shared.Database.Mapping.Settings.SettingsRegistry where

import Database.PostgreSQL.Simple

import Shared.Model.Settings.Settings

instance ToRow SettingsRegistry

instance FromRow SettingsRegistry
