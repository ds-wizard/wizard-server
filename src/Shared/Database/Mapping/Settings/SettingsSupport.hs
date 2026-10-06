module Shared.Database.Mapping.Settings.SettingsSupport where

import Database.PostgreSQL.Simple

import Shared.Model.Settings.Settings

instance ToRow SettingsSupport

instance FromRow SettingsSupport
