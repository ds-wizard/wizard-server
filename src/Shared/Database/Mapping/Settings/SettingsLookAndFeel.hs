module Shared.Database.Mapping.Settings.SettingsLookAndFeel where

import Database.PostgreSQL.Simple

import Shared.Model.Settings.Settings

instance ToRow SettingsLookAndFeel

instance FromRow SettingsLookAndFeel
