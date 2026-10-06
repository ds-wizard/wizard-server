module Shared.Database.Mapping.Settings.SettingsFeatures where

import Database.PostgreSQL.Simple

import Shared.Model.Settings.Settings

instance ToRow SettingsFeatures

instance FromRow SettingsFeatures
