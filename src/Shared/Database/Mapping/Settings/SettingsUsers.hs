module Shared.Database.Mapping.Settings.SettingsUsers where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Model.Settings.Settings

instance ToRow SettingsUsers where
  toRow SettingsUsers {..} = [toField . PGArray $ affiliations]

instance FromRow SettingsUsers where
  fromRow = SettingsUsers . fromPGArray <$> field
