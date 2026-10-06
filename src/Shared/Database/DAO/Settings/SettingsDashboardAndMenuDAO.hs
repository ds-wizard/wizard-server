module Shared.Database.DAO.Settings.SettingsDashboardAndMenuDAO where

import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsAnnouncement ()
import Shared.Database.Mapping.Settings.SettingsCustomMenuLink ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_dashboard_and_menu"

customMenuLinkTable :: String
customMenuLinkTable = "settings_dashboard_and_menu_custom_menu_link"

announcementTable :: String
announcementTable = "settings_dashboard_and_menu_announcement"

childTables :: [String]
childTables = [customMenuLinkTable, announcementTable]

findSettingsDashboardAndMenu :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsDashboardAndMenu)
findSettingsDashboardAndMenu tenantUuid mWorkspaceUuid = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  mRow <- findSettingsRow ["workspace_override_allowed"] key
  case mRow :: Maybe (Only Bool) of
    Nothing -> return Nothing
    Just _ -> do
      customMenuLinks <- findSettingsChildRows ["icon", "title", "url", "new_window"] (childKey customMenuLinkTable key)
      announcements <- findSettingsChildRows ["content", "level"] (childKey announcementTable key)
      return . Just $ SettingsDashboardAndMenu {customMenuLinks = customMenuLinks, announcements = announcements}

saveSettingsDashboardAndMenu :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsDashboardAndMenu -> m ()
saveSettingsDashboardAndMenu tenantUuid mWorkspaceUuid settings = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  saveSettingsRow [] key ()
  replaceSettingsChildRows ["icon", "title", "url", "new_window"] (childKey customMenuLinkTable key) settings.customMenuLinks
  replaceSettingsChildRows ["content", "level"] (childKey announcementTable key) settings.announcements
