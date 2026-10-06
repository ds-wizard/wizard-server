module Shared.Database.DAO.Settings.SettingsLoginScreenDAO where

import qualified Data.UUID as U

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsAnnouncement ()
import Shared.Database.Mapping.Settings.SettingsLoginScreen ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_login_screen"

announcementTable :: String
announcementTable = "settings_login_screen_announcement"

findSettingsLoginScreen :: WizardRequestContextC s m => U.UUID -> m (Maybe SettingsLoginScreen)
findSettingsLoginScreen tenantUuid = do
  let key = orgKey table tenantUuid
  mRow <- findSettingsRow ["login_info", "login_info_sidebar"] key
  announcements <- findSettingsChildRows ["content", "level"] (childKey announcementTable key)
  return $ fmap (\(loginInfo, loginInfoSidebar) -> SettingsLoginScreen {..}) mRow

saveSettingsLoginScreen :: WizardRequestContextC s m => U.UUID -> SettingsLoginScreen -> m ()
saveSettingsLoginScreen tenantUuid settings = do
  let key = orgKey table tenantUuid
  saveSettingsRow ["login_info", "login_info_sidebar"] key settings
  replaceSettingsChildRows ["content", "level"] (childKey announcementTable key) settings.announcements
