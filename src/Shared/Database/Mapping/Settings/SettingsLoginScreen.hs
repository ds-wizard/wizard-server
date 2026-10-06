module Shared.Database.Mapping.Settings.SettingsLoginScreen where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Settings.Settings

instance ToRow SettingsLoginScreen where
  toRow SettingsLoginScreen {..} = [toField loginInfo, toField loginInfoSidebar]
