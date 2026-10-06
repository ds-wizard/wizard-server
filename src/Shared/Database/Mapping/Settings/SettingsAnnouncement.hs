module Shared.Database.Mapping.Settings.SettingsAnnouncement where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Settings.Settings

instance ToRow SettingsAnnouncement

instance FromRow SettingsAnnouncement

instance ToField SettingsAnnouncementLevelType where
  toField = toFieldGenericEnum

instance FromField SettingsAnnouncementLevelType where
  fromField = fromFieldGenericEnum
