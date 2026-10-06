module Shared.Database.Mapping.Settings.SettingsRoles where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Settings.Settings

instance ToRow SettingsRoles where
  toRow SettingsRoles {..} = [toField defaultRoleUuid]

instance FromRow SettingsRoles where
  fromRow = SettingsRoles <$> field
